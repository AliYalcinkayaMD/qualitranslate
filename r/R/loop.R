# Cooperative event loop.
#
# R is single-threaded. The web server (httpuv) only answers requests while R runs its
# event loop, so every long wait in the app (a translation job, the Copilot sign-in watcher,
# each wait inside the Copilot transport, API requests) goes through qt_wait(), which keeps
# serving requests and driving the asynchronous curl transfers in small slices. The browser
# interface therefore keeps polling /api/state once a second, and /api/stop and
# /api/copilot/cancel take effect during a multi-minute run.
#
# Request handlers never wait: anything slow is queued and picked up by the main loop
# (qt_loop_once) outside any httpuv/later callback. qt_wait() refuses to run inside a
# handler, because servicing the event loop from inside one of its own callbacks is unsafe.

qt_pool <- function() {
  if (is.null(.qt$pool)) .qt$pool <- curl::new_pool(total_con = 20, host_con = 6)
  .qt$pool
}

# Drive the asynchronous curl transfers without blocking.
qt_tick <- function() {
  pool <- qt_pool()
  if (length(curl::multi_list(pool))) curl::multi_run(timeout = 0, poll = FALSE, pool = pool)
  invisible()
}

curl_busy <- function() length(curl::multi_list(qt_pool())) > 0L

qt_service <- function(seconds) {
  # httpuv::service(0) would wait forever, so always pass at least one millisecond.
  httpuv::service(max(1, round(seconds * 1000)))
}

#' @noRd
qt_wait <- function(seconds = 0) {
  if (isTRUE(.qt$in_handler)) stop("internal error: a request handler tried to wait", call. = FALSE)
  end <- qt_clock() + seconds
  repeat {
    qt_tick()
    left <- end - qt_clock()
    if (left <= 0) break
    qt_service(min(left, if (curl_busy()) 0.01 else 0.05))
  }
  if (seconds <= 0) later::run_now(0)
  qt_tick()
  invisible()
}

# One pass of the main loop: run queued work, or serve requests for up to `timeout` seconds.
qt_loop_once <- function(ws, timeout = 0.1) {
  if (!is.null(ws$pending_job)) {
    ws_run_job(ws)
    return(invisible())
  }
  if (length(ws$deferred)) {
    action <- ws$deferred[[1]]
    ws$deferred <- ws$deferred[-1]
    action()
    return(invisible())
  }
  conn <- ws$copilot_connection
  if (isTRUE(conn$due())) {
    conn$tick()
    return(invisible())
  }
  wait <- timeout
  if (isTRUE(conn$active)) wait <- max(0.001, min(wait, conn$next_at - qt_clock()))
  qt_wait(wait)
  invisible()
}

# Asynchronous HTTP request through the shared curl pool. `done` receives the curl response,
# `fail` the error message. The transfer advances whenever qt_tick() runs.
qt_fetch_async <- function(url, handle, done, fail) {
  curl::curl_fetch_multi(url, done = done, fail = fail, pool = qt_pool(), handle = handle)
  invisible()
}

# Wait (cooperatively) for an asynchronous request.
qt_fetch_wait <- function(url, handle) {
  outcome <- NULL
  qt_fetch_async(url, handle,
                 done = function(res) outcome <<- list(ok = TRUE, res = res),
                 fail = function(msg) outcome <<- list(ok = FALSE, msg = msg))
  while (is.null(outcome)) qt_wait(0.005)
  outcome
}
