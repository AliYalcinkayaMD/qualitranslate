"""QualiTranslate: forward translation, blind back-translation, reconciliation and
human review of qualitative interview transcripts, in a local browser workspace."""
from .core import APP_VERSION as __version__

__all__ = ["__version__", "main"]


def main(argv=None):
    """Start the local QualiTranslate workspace (same as the `qualitranslate` command)."""
    from .app import main as _main
    return _main(argv)
