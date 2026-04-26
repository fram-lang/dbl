import subprocess
import time

try:
    from .common import TIMEOUT, parse_expectations, find_expectation_failure
except ImportError:
    from common import TIMEOUT, parse_expectations, find_expectation_failure

def run_program_test(binary, flags, file, expect_exit, timeout=TIMEOUT):
    """Runs program test and returns structured pass/fail result.
    
    Args:
        binary: Path to executable
        flags: Command line flags
        file: Test file path
        expect_exit: Expected exit code
        timeout: Timeout in seconds (default: TIMEOUT constant)
    """
    cmd = f"{binary} {flags} {file}"
    expectations = parse_expectations(file)

    try:
        start_time = time.time()
        proc = subprocess.run(
            cmd,
            shell=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired:
        elapsed = time.time() - start_time
        return {
            "passed": False,
            "message": f"TIMEOUT: {file} (exceeded {timeout}s timeout, ran for {elapsed:.2f}s)",
        }

    # Check exit code
    if proc.returncode != expect_exit:
        stderr = proc.stderr.strip()
        details = f"\n{stderr}" if stderr else ""
        return {
            "passed": False,
            "message": f"BAD EXIT: {file} got {proc.returncode} expected {expect_exit}{details}",
        }

    # Check expectations
    failure = find_expectation_failure(expectations, proc.stdout, proc.stderr)
    if failure is not None:
        return {"passed": False, "message": failure}

    return {"passed": True, "message": None}