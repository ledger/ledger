import sys
import os

# Verify that sys.argv[0] is the script filename, not the ledger executable.
# When Py_Main() was called after Py_Initialize(), it would set argv[0] to
# the ledger binary path.  With the fix using PySys_SetArgvEx, argv[0] is
# the script name itself, matching CPython's own behaviour.
script_name = os.path.basename(sys.argv[0])
assert script_name == '819.py', \
    "Expected sys.argv[0] to be '819.py', got: %s" % sys.argv[0]

# Verify that importing readline (which IPython and similar tools do) does not
# crash.  Before the fix, Py_Main() would reinitialise readline after
# Py_Initialize() had already set it up, leading to a segfault when a second
# tool (such as IPython's IPShellEmbed) tried to use readline state.
try:
    import readline  # noqa: F401 – just testing that the import doesn't crash
except ImportError:
    pass  # readline may be absent on some platforms; that is fine

print("OK")
