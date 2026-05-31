import pexpect
import shlex

try:
    from .common import TIMEOUT, PROMPT, parse_repl_tests, find_expectation_failure
except ImportError:
    from common import TIMEOUT, PROMPT, parse_repl_tests, find_expectation_failure

STDERR_INDICATORS = ["error", "warning", "exception", "traceback", "fatal", "fail", "note", "segmentation fault", "core dumped"]

def _run_single_test(child, test, timeout):
    """Runs a single REPL test, returns (passed, failure_message)."""
    try:        
        # Send the test code to the REPL
        child.sendline(test["code"])
        
        # Wait for prompt
        child.expect(r"(?m)^[ ]*>", timeout=timeout)
        
        output = child.before
        
        #Let's assume output is on stderror if it contains "error" or "warning", otherwise it's on stdout. This is a simplification.
        if any(indicator in output.lower() for indicator in STDERR_INDICATORS):
            stderr_text = output
            stdout_text = ""
        else:
            stdout_text = output
            stderr_text = ""
        
        failure = find_expectation_failure(test["expect"], stdout_text, stderr_text)
        if failure is not None:
            return False, failure
        return True, None
    except pexpect.TIMEOUT as _:
        return False, f"[TIMEOUT] Test '{test['code']}'"
    except Exception as e:
        return False, f"[ERROR] Test '{test['code']}': {e}"


def _failure_entry(file, command, message):
    return {
        "file": file,
        "command": command,
        "message": message or "[UNKNOWN ERROR]",
    }

def run_repl_test(binary, flags, file, timeout=TIMEOUT):
    """Runs REPL tests from file, returns structured result with failures."""
    args = [binary] + shlex.split(flags)
    
    # We will spawn the REPL once and run all tests in the file sequentially. 
    command = args[0]
    cargs = args[1:]
    
    failures = []
    
    try:
        child = pexpect.spawn(command, args=cargs, encoding='utf-8', timeout=timeout)
        child.setecho(False)  # Disable echo to avoid duplicated input in output
        tests = parse_repl_tests(file)
        
        # consume initial prompt
        child.expect(PROMPT, timeout=timeout)
        
        for index, test in enumerate(tests, start=1):
            passed, failure = _run_single_test(child, test, timeout)
            if not passed:
                failures.append(_failure_entry(file, index, failure))

        return {"passed": not failures, "failures": failures}
    finally:
        if 'child' in locals():
            child.close(force=True)
