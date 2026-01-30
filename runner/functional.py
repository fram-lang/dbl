import subprocess
import time

from common import TIMEOUT, parse_expectations, check_expectations

def run_program_test(binary, flags, file, expect_exit, timeout=TIMEOUT):
    """Runs program test and checks exit code and output expectations.
    
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
        print(f"TIMEOUT: {file} (exceeded {timeout}s timeout, ran for {elapsed:.2f}s)")
        return False

    # Check exit code
    if proc.returncode != expect_exit:
        print(f"BAD EXIT: {file} got {proc.returncode} expected {expect_exit}")
        print(proc.stderr)
        return False

    # Check expectations
    return check_expectations(expectations, proc.stdout, proc.stderr)