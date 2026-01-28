import subprocess
import time
import shlex

from common import TIMEOUT, PROMPT, parse_tests, check_expectations

def read_until_prompt(proc, timeout=TIMEOUT):
    """Reads output until prompt is found or timeout occurs."""
    output = ""
    start_time = time.time()
    
    while time.time() - start_time < timeout:
        try:
            c = proc.stdout.read(1)
            if not c:  # EOF
                break
            output += c
            if output.endswith(PROMPT):
                return output
        except Exception:
            break
    
    elapsed = time.time() - start_time
    raise TimeoutError(f"Timeout waiting for prompt after {timeout}s (elapsed: {elapsed:.2f}s, got {len(output)} chars)")

def maybe_read_stderr(proc, timeout=0.1):
    """Attempts to read stderr with timeout."""
    try:
        import select
        ready, _, _ = select.select([proc.stderr], [], [], timeout)
        return proc.stderr.readline() if ready else ""
    except Exception:
        return ""

def _run_single_test(proc, test, timeout):
    """Runs a single REPL test, returns True if passes."""
    try:
        proc.stdin.write(test["code"] + "\n")
        proc.stdin.flush()
        
        stdout = read_until_prompt(proc, timeout=timeout)
        stderr = maybe_read_stderr(proc, timeout=0.1)
        
        return check_expectations(test["expect"], stdout, stderr)
    except TimeoutError as e:
        print(f"[TIMEOUT] Test '{test['code']}': {e}")
        return False
    except Exception as e:
        print(f"[ERROR] Test '{test['code']}': {e}")
        return False


def _close_process(proc, timeout):
    """Gracefully closes REPL process."""
    try:
        proc.terminate()
        proc.wait(timeout=timeout)
    except Exception:
        proc.kill()

def run_repl_test(binary, flags, file, timeout=TIMEOUT):
    """Runs REPL tests from file, returns True if all pass."""
    args = [binary] + shlex.split(flags)
    
    proc = subprocess.Popen(
        args,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1
    )

    try:
        tests = parse_tests(file)
        read_until_prompt(proc, timeout=timeout)  # consume initial prompt

        for test in tests:
            if not _run_single_test(proc, test, timeout):
                return False

        return True
    finally:
        _close_process(proc, timeout)