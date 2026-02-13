import subprocess
import sys
import glob as _glob

from functional import run_program_test
from repl import run_repl_test


class TestRunner:
    """Manages test execution and result tracking."""
    
    def __init__(self, binary):
        self.binary = binary
        self.flags = ""
        self.total = 0
        self.passed = 0
    
    @staticmethod
    def glob(pattern):
        """Returns sorted list of files matching pattern."""
        return sorted(_glob.glob(pattern))
    
    def _record_test(self, success):
        """Records test result."""
        self.total += 1
        if success:
            self.passed += 1
    
    def with_flags(self, flags_str, test_fn):
        """Temporarily changes flags for test execution."""
        old_flags = self.flags
        self.flags = flags_str
        try:
            test_fn()
        finally:
            self.flags = old_flags
    
    def simple(self, file):
        """Runs test expecting exit code 0."""
        result = run_program_test(self.binary, self.flags, file, expect_exit=0)
        self._record_test(result)
    
    def exit_code(self, code, file):
        """Runs test expecting specific exit code."""
        result = run_program_test(self.binary, self.flags, file, expect_exit=code)
        self._record_test(result)
    
    def repl(self, file):
        """Runs REPL test."""
        result = run_repl_test(self.binary, self.flags, file)
        self._record_test(result)

    def simple_run_tests(self, test_files_name, flags=""):
        old_flags = self.flags
        self.flags = flags
        try:
            for file in TestRunner.glob(test_files_name):
                self.simple(file)
        finally:
            self.flags = old_flags
    
    def repl_tests(self, test_files_name, flags=""):
        old_flags = self.flags
        self.flags = flags
        try:
            for file in TestRunner.glob(test_files_name):
                print(f"\n--- REPL TEST: {file} ---\n")
                self.repl(file)
        finally:
            self.flags = old_flags



def _build_target(program_name):
    """Builds target using dune, returns path or None on failure."""
    target = f"src/{program_name}.exe"
    if subprocess.run(["dune", "build", target]).returncode != 0:
        return None
    return f"_build/default/{target}"


def main():
    """Main entry point for test runner."""
    if len(sys.argv) != 3:
        print("USAGE: runner.py PROGRAM TEST_SUITE")
        return 1

    binary = _build_target(sys.argv[1])
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
    }

    with open(sys.argv[2]) as f:
        exec(f.read(), namespace)

    print(f"\nPassed: {runner.passed}/{runner.total}")
    return 0 if runner.passed == runner.total else 1


if __name__ == "__main__":
    sys.exit(main())
