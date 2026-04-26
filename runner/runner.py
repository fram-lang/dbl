import subprocess
import sys
import glob as _glob
import shutil

try:
    from .functional import run_program_test
    from .repl import run_repl_test
except ImportError:
    from functional import run_program_test
    from repl import run_repl_test


class Colors:
    RESET = "\033[0m"
    BOLD = "\033[1m"
    DIM = "\033[2m"
    RED = "\033[31m"
    GREEN = "\033[32m"
    YELLOW = "\033[33m"
    CYAN = "\033[36m"


def tqdm(iterable, desc=""):
    items = list(iterable)
    total = len(items)
    label = f"{Colors.CYAN}{desc}{Colors.RESET}" if desc else "Tests"
    width = shutil.get_terminal_size(fallback=(120, 20)).columns

    def print_progress(text):
        clean_text = text[: max(1, width - 1)]
        padded = clean_text.ljust(max(1, width - 1))
        print(f"\r{padded}", end="", flush=True)

    for index, item in enumerate(items, start=1):
        print_progress(f"{label} {Colors.DIM}[{index}/{total}]{Colors.RESET}")
        yield item

    if total > 0:
        print()


def _print_summary(passed, total):
    failed = total - passed
    success = passed == total

    status_text = "ALL TESTS PASSED" if success else "TESTS FAILED"
    status_color = Colors.GREEN if success else Colors.RED
    ratio_color = Colors.GREEN if success else Colors.YELLOW

    border = f"{status_color}{Colors.BOLD}{'=' * 48}{Colors.RESET}"
    title = f"{status_color}{Colors.BOLD}  {status_text:^44}  {Colors.RESET}"
    ratio = (
        f"{Colors.BOLD}Passed:{Colors.RESET} "
        f"{ratio_color}{Colors.BOLD}{passed}/{total}{Colors.RESET}"
    )
    failed_line = (
        f"{Colors.BOLD}Failed:{Colors.RESET} "
        f"{(Colors.RED if failed else Colors.GREEN)}{Colors.BOLD}{failed}{Colors.RESET}"
    )

    print()
    print(border)
    print(title)
    print(border)
    print(f"  {ratio}")
    print(f"  {failed_line}")
    print(border)


def _print_failures_report(title, failures):
    if not failures:
        return

    border = f"{Colors.RED}{Colors.BOLD}{'-' * 72}{Colors.RESET}"
    print()
    print(border)
    print(f"{Colors.RED}{Colors.BOLD}{title}{Colors.RESET}")
    print(border)

    for index, failure in enumerate(failures, start=1):
        command_info = failure.get("command")
        command_suffix = f" (command {command_info})" if command_info is not None else ""
        print(
            f"{Colors.YELLOW}{Colors.BOLD}[{index}] [{failure['kind']}] "
            f"{failure['file']}{command_suffix}{Colors.RESET}"
        )
        if failure.get("message"):
            print(failure["message"])
        print(border)


class TestRunner:
    """Manages test execution and result tracking."""
    
    def __init__(self, binary):
        self.binary = binary
        self.flags = ""
        self.total = 0
        self.passed = 0
        self.deferred_failures = []
    
    @staticmethod
    def glob(pattern):
        """Returns sorted list of files matching pattern."""
        return sorted(_glob.glob(pattern))
    
    def _record_test(self, success):
        """Records test result."""
        self.total += 1
        if success:
            self.passed += 1

    def _record_failure(self, kind, file, message, command=None):
        self.deferred_failures.append({
            "kind": kind,
            "file": file,
            "message": message,
            "command": command,
        })

    def _flush_failures_since(self, start_index, title):
        pending = self.deferred_failures[start_index:]
        if pending:
            _print_failures_report(title, pending)

    def _run_single_test(self, kind, file, runner_fn):
        try:
            result = runner_fn()
        except Exception as err:
            self._record_failure(kind, file, f"[ERROR] {kind} test crashed: {file}\n{err}")
            self._record_test(False)
            return

        passed = bool(result.get("passed", False))
        if not passed:
            self._collect_failures(kind, file, result)
        self._record_test(passed)

    def _collect_failures(self, kind, file, result):
        if kind == "repl":
            failures = result.get("failures", [])
            for failure in failures:
                self._record_failure(
                    "repl",
                    failure.get("file", file),
                    failure.get("message"),
                    failure.get("command"),
                )
            return

        self._record_failure(kind, file, result.get("message"))
    
    def with_flags(self, flags_str, test_fn):
        """Temporarily changes flags for test execution."""
        old_flags = self.flags
        start_index = len(self.deferred_failures)
        self.flags = flags_str
        try:
            test_fn()
        finally:
            self.flags = old_flags

        section_title = f"FAILURES (with_flags: {flags_str or '<none>'})"
        self._flush_failures_since(start_index, section_title)
    
    def simple(self, file):
        """Runs test expecting exit code 0."""
        self._run_single_test(
            kind="simple",
            file=file,
            runner_fn=lambda: run_program_test(self.binary, self.flags, file, expect_exit=0),
        )
    
    def exit_code(self, code, file):
        """Runs test expecting specific exit code."""
        self._run_single_test(
            kind="exit_code",
            file=file,
            runner_fn=lambda: run_program_test(self.binary, self.flags, file, expect_exit=code),
        )
    
    def repl(self, file):
        """Runs REPL test."""
        self._run_single_test(
            kind="repl",
            file=file,
            runner_fn=lambda: run_repl_test(self.binary, self.flags, file),
        )

    def simple_run_tests(self, test_files_name, flags=""):
        old_flags = self.flags
        start_index = len(self.deferred_failures)
        self.flags = flags
        try:
            for file in tqdm(TestRunner.glob(test_files_name), desc=f"Simple Tests {test_files_name}"):
                self.simple(file)
        finally:
            self.flags = old_flags

        self._flush_failures_since(start_index, f"FAILURES - Simple Tests {test_files_name}")
    
    def repl_tests(self, test_files_name, flags=""):
        old_flags = self.flags
        start_index = len(self.deferred_failures)
        self.flags = flags
        try:
            for file in tqdm(TestRunner.glob(test_files_name), desc=f"REPL Tests {test_files_name}"):
                self.repl(file)
        finally:
            self.flags = old_flags

        self._flush_failures_since(start_index, f"FAILURES - REPL Tests {test_files_name}")



def _build_target():
    """Builds dbl target using dune, returns path or None on failure."""
    target = "src/dbl.exe"
    if subprocess.run(["dune", "build", target]).returncode != 0:
        return None
    return f"_build/default/{target}"


def main():
    """Main entry point for test runner."""
    if len(sys.argv) != 2:
        print("USAGE: runner.py TEST_SUITE")
        return 1

    binary = _build_target()
    if not binary:
        return 1

    runner = TestRunner(binary)
    
    # Create namespace with runner methods for test suite execution
    namespace = {
        "simple": runner.simple,
        "exit_code": runner.exit_code,
        "with_flags": runner.with_flags,
        "glob": TestRunner.glob,
        "repl": runner.repl,
        "simple_run_tests": runner.simple_run_tests,
        "repl_tests": runner.repl_tests,
        "tqdm": tqdm
    }

    with open(sys.argv[1]) as f:
        exec(f.read(), namespace)

    _print_summary(runner.passed, runner.total)
    return 0 if runner.passed == runner.total else 1


if __name__ == "__main__":
    sys.exit(main())
