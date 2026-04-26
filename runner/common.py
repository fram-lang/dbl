"""Common utilities for test runners."""

# Default timeout for tests (in seconds)
TIMEOUT = 5.0

# REPL prompt
PROMPT = ">"

EXP_PREFIX = "# @"

REPL_END_MARKER = ";;"


def _new_expectations():
    return {
        "stdout": [],
        "stderr": [],
        "stdout-empty": False,
        "stderr-empty": False,
    }

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
    if not line.startswith(EXP_PREFIX):
        return None, None
    
    rest = line[3:].strip()
    
    # Check for empty expectations
    if rest in ("stdout-empty", "stderr-empty"):
        return rest, ""
    
    if ":" not in rest:
        return None, None
    
    kind, text = rest.split(":", 1)
    return kind.strip(), text.strip()

def parse_repl_tests(path):
    """Parses REPL test file with multiline code blocks and expectations.

    Rules:
      - Code can span multiple lines.
      - `# @...` lines are expectations for the current command.
      - A new command starts when we hit a code line after a completed command
        (completed means code ended with `;;` and had expectations attached).
      - If command doesn't end with `;;`, parser appends it automatically.
    """
    tests = []
    current_test = None
    code_lines = []

    def finish_current():
        nonlocal current_test, code_lines
        if current_test is None:
            return

        code = "\n".join(code_lines).rstrip()
        if code and not code.endswith(REPL_END_MARKER):
            code = f"{code}\n{REPL_END_MARKER}"
        current_test["code"] = code
        tests.append(current_test)

        current_test = None
        code_lines = []

    with open(path) as f:
        for raw_line in f:
            line = raw_line.rstrip("\n")
            stripped = line.strip()

            if not stripped:
                continue

            kind, text = _parse_expectation_line(stripped)
            if kind is not None:
                if current_test is None:
                    continue
                if kind in ("stdout-empty", "stderr-empty"):
                    current_test["expect"][kind] = True
                elif kind in current_test["expect"]:
                    current_test["expect"][kind].append(text)
                continue

            if current_test is None:
                current_test = {"code": "", "expect": _new_expectations()}
                code_lines = [line]
                continue

            previous_code = "\n".join(code_lines).rstrip()
            previous_done = previous_code.endswith(REPL_END_MARKER)
            has_expectations = (
                bool(current_test["expect"]["stdout"])
                or bool(current_test["expect"]["stderr"])
                or current_test["expect"]["stdout-empty"]
                or current_test["expect"]["stderr-empty"]
            )

            if has_expectations:
                if stripped == REPL_END_MARKER and not previous_done:
                    code_lines.append(line)
                else:
                    finish_current()
                    current_test = {"code": "", "expect": _new_expectations()}
                    code_lines = [line]
            else:
                code_lines.append(line)

    finish_current()
    return tests


def parse_expectations(file):
    expectations = _new_expectations()

    with open(file) as f:
        for raw_line in f:
            line = raw_line.strip()
            if not line:
                continue

            kind, text = _parse_expectation_line(line)
            if kind is None:
                continue

            if kind in ("stdout-empty", "stderr-empty"):
                expectations[kind] = True
            elif kind in expectations:
                expectations[kind].append(text)

    return expectations


def find_expectation_failure(expectations, stdout, stderr):
    if expectations["stdout-empty"] and stdout.strip():
        return "\n".join([
            "[FAILED stdout-empty]",
            "  expected: empty stdout",
            "  actual:",
            stdout,
        ])

    if expectations["stderr-empty"] and stderr.strip():
        return "\n".join([
            "[FAILED stderr-empty]",
            "  expected: empty stderr",
            "  actual:",
            stderr,
        ])

    for expected in expectations["stdout"]:
        if expected not in stdout:
            return "\n".join([
                "[FAILED stdout]",
                f"  expected substring: {expected!r}",
                "  actual stdout:",
                stdout,
            ])

    for expected in expectations["stderr"]:
        if expected not in stderr:
            return "\n".join([
                "[FAILED stderr]",
                f"  expected substring: {expected!r}",
                "  actual stderr:",
                stderr,
            ])

    return None

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
    failure = find_expectation_failure(expectations, stdout, stderr)
    if failure is not None:
        print(failure)
        return False
    return True

