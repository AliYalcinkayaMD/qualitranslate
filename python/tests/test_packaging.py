"""Packaging and cross-copy safety checks."""
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'src'))
from qualitranslate import __version__
from qualitranslate.copilot_browser import BrowserError, CopilotBrowser
from qualitranslate.core import APP_VERSION


class SharedAssetTests(unittest.TestCase):
    @unittest.skipUnless((ROOT / 'tools' / 'sync_shared.py').exists(), 'only inside the source repository')
    def test_package_copies_match_shared_sources(self):
        result = subprocess.run([sys.executable, str(ROOT / 'tools' / 'sync_shared.py'), '--check'], capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stdout)

    def test_versions_agree(self):
        self.assertEqual(__version__, APP_VERSION)
        pyproject = Path(__file__).resolve().parents[1] / 'pyproject.toml'
        if pyproject.exists():
            self.assertIn('version = "' + APP_VERSION + '"', pyproject.read_text())
        page = Path(__file__).resolve().parents[1] / 'src' / 'qualitranslate' / 'web' / 'index.html'
        self.assertIn('<span>' + APP_VERSION + ' ·', page.read_text(encoding='utf-8'))


@unittest.skipIf(sys.platform.startswith('win'), 'POSIX record locks')
class ProfileLockTests(unittest.TestCase):
    def test_second_process_cannot_use_the_same_copilot_profile(self):
        with tempfile.TemporaryDirectory() as tmp:
            holder = subprocess.Popen([sys.executable, '-c', (
                'import sys,time;sys.path.insert(0,%r);from pathlib import Path;from qualitranslate.copilot_browser import CopilotBrowser;'
                'b=CopilotBrowser(profile_dir=%r);b._acquire(Path(%r));print("locked",flush=True);time.sleep(30)') % (str(Path(__file__).resolve().parents[1] / 'src'), tmp, tmp)],
                stdout=subprocess.PIPE, text=True)
            try:
                self.assertEqual(holder.stdout.readline().strip(), 'locked')
                with self.assertRaises(BrowserError) as ctx:
                    CopilotBrowser(profile_dir=tmp)._acquire(Path(tmp))
                self.assertEqual(ctx.exception.code, 'in_use')
            finally:
                holder.kill(); holder.wait()
            time.sleep(0.2)
            browser = CopilotBrowser(profile_dir=tmp); browser._acquire(Path(tmp)); browser._release()


if __name__ == '__main__':
    unittest.main()
