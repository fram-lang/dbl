"""Common utilities for test runners."""

# Default timeout for tests (in seconds)
TIMEOUT = 5.0

# REPL prompt
PROMPT = ">"


def _parse_expectation_line(line):
    """Parses a single expectation line (# @...).
    
    Supports:
      # @stdout: text         - expect text in stdout
      # @stderr: text         - expect text in stderr
      # @stdout-empty         - expect empty stdout
      # @stderr-empty         - expect empty stderr
    
    Args:
        line: Line to parse
        
    Returns:
        Tuple (kind, text) or (None, None) if not an expectation line
    """
    if not line.startswith("# @"):
        return None, None
    
    rest = line[3:].strip()
    
    # Check for empty expectations
    if rest in ("stdout-empty", "stderr-empty"):
        return rest, ""
    
    if ":" not in rest:
        return None, None
    
    kind, text = rest.split(":", 1)
    return kind.strip(), text.strip()

def parse_tests(path):
    """Parses REPL test file with code lines and # @ expectations.
    
    Format:
      code_line_1
      # @stdout: expected_output
      # @stderr: expected_error
      # @stdout-empty          (expect empty stdout)
      # @stderr-empty          (expect empty stderr)
      code_line_2
      ...
    
    Args:
        path: Path to test file
        
    Returns:
        List of tests, each with 'code' and 'expect' keys
    """
    tests = []
    current_test = None

    with open(path) as f:
        for line in f:
            line = line.rstrip()
            if not line:
                continue

            # New code line = new test
            if not line.startswith("#"):
                if current_test:
                    tests.append(current_test)
                current_test = {
                    "code": line,
                    "expect": {"stdout": [], "stderr": [], "stdout-empty": False, "stderr-empty": False}
                }
            # Expected output annotation
            elif current_test:
                kind, text = _parse_expectation_line(line)
                if kind:
                    if kind in ("stdout-empty", "stderr-empty"):
                        current_test["expect"][kind] = True
                    elif kind in current_test["expect"]:
                        current_test["expect"][kind].append(text)

    if current_test:
        tests.append(current_test)
    return tests

def parse_expectations(file):
    tests = parse_tests(file)
    expectations = {"stdout": [], "stderr": [], "stdout-empty": False, "stderr-empty": False}
    for test in tests:
        expectations["stdout"].extend(test["expect"]["stdout"])
        expectations["stderr"].extend(test["expect"]["stderr"])
        expectations["stdout-empty"] = expectations["stdout-empty"] or test["expect"]["stdout-empty"]
        expectations["stderr-empty"] = expectations["stderr-empty"] or test["expect"]["stderr-empty"]

    return expectations

def check_expectations(expectations, stdout, stderr):
    """Checks if output matches expectations for both stdout and stderr.
    
    Supports:
      - Checking for presence of text in stdout/stderr
      - Checking for empty stdout/stderr
    
    Args:
        expectations: Dictionary with 'stdout', 'stderr', 'stdout-empty', 'stderr-empty' keys
        stdout: Actual stdout output
        stderr: Actual stderr output
        
    Returns:
        True if all expectations are met, False otherwise
    """
    # Check stdout-empty
    if expectations["stdout-empty"]:
        if stdout.strip():
            print(f"[FAILED] Expected empty stdout, but got:\n{stdout}")
            return False
    
    # Check stderr-empty
    if expectations["stderr-empty"]:
        if stderr.strip():
            print(f"[FAILED] Expected empty stderr, but got:\n{stderr}")
            return False
    
    # Check stdout contains expected text
    for expected in expectations["stdout"]:
        if expected not in stdout:
            print(f"[FAILED stdout] '{expected}' not in:\n{stdout}")
            return False
    
    for expected in expectations["stderr"]:
        if expected not in stderr:
            print(f"[FAILED stderr] '{expected}' not in:\n{stderr}")
            return False
    
    return True

