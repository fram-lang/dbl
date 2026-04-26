import subprocess
import time
import shlex

try:
    from .common import TIMEOUT, PROMPT, parse_repl_tests, find_expectation_failure
except ImportError:
    from common import TIMEOUT, PROMPT, parse_repl_tests, find_expectation_failure

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
    """Reads all currently available stderr output with timeout."""
    try:
        import select
        chunks = []

        ready, _, _ = select.select([proc.stderr], [], [], timeout)
        while ready:
            line = proc.stderr.readline()
            if not line:
                break
            chunks.append(line)
            ready, _, _ = select.select([proc.stderr], [], [], 0.01)

        return "".join(chunks)
    except Exception:
        return ""

def _run_single_test(proc, test, timeout):
    """Runs a single REPL test, returns (passed, failure_message)."""
    try:
        proc.stdin.write(test["code"] + "\n")
        proc.stdin.flush()
        
        stdout = read_until_prompt(proc, timeout=timeout)
        stderr = maybe_read_stderr(proc, timeout=0.1)

        failure = find_expectation_failure(test["expect"], stdout, stderr)
        if failure is not None:
            return False, failure
        return True, None
    except TimeoutError as e:
        return False, f"[TIMEOUT] Test '{test['code']}': {e}"
    except Exception as e:
        return False, f"[ERROR] Test '{test['code']}': {e}"


def _failure_entry(file, command, message):
    return {
        "file": file,
        "command": command,
        "message": message or "[UNKNOWN ERROR]",
    }


def _close_process(proc, timeout):
    """Gracefully closes REPL process."""
    try:
        proc.terminate()
        proc.wait(timeout=timeout)
    except Exception:
        proc.kill()

def run_repl_test(binary, flags, file, timeout=TIMEOUT):
    """Runs REPL tests from file, returns structured result with failures."""
    args = [binary] + shlex.split(flags)
    
    proc = subprocess.Popen(
        args,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1
    )

    failures = []

    try:
        tests = parse_repl_tests(file)
        read_until_prompt(proc, timeout=timeout)  # consume initial prompt

        for index, test in enumerate(tests, start=1):
            passed, failure = _run_single_test(proc, test, timeout)
            if not passed:
                failures.append(_failure_entry(file, index, failure))

        return {"passed": not failures, "failures": failures}
    finally:
        _close_process(proc, timeout)