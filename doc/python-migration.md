# Migrating from `import ledger` to `lpy`

This guide is for users who have Python scripts that use the old
`import ledger` API and want to migrate to the new `lpy` package.

## Background

Ledger's Python bindings were historically exposed as a module named
`ledger`, mirroring the CLI tool's name.  This caused two practical
problems:

1. **Build-directory collision** — the compiled `ledger` binary and the
   `ledger/` Python package directory cannot coexist in the same
   directory, complicating the build.
2. **Non-extensibility** — a flat C extension module cannot be augmented
   with pure-Python code without modifying the C++ layer or resorting
   to monkey-patching.

The new `lpy` package solves both problems.  The C++ extension is now a
private module named `_core`, wrapped by the `lpy.core` Python package.
This separation lets the package grow with pure-Python additions while
keeping the C++ layer focused and stable.

## Package structure

```
lpy/                        # top-level package
    __init__.py             # exposes lpy.core
    core/
        __init__.py         # re-exports everything from _core;
        _core.so            # native C++ extension
ledger/
    __init__.py             # shim: from lpy.core import *
```

## What changed

| Old (`import ledger`)         | New (`from lpy import core`)     |
|-------------------------------|----------------------------------|
| `import ledger`               | `from lpy import core`           |
| `ledger.Amount("$10")`        | `core.Amount("$10")`             |
| `ledger.Balance()`            | `core.Balance()`                 |
| `ledger.read_journal(path)`   | `core.read_journal(path)`        |
| `ledger.commodities`          | `core.commodities`               |
| `from ledger import Amount`   | `from lpy.core import Amount`    |

Every public name that was previously available under `ledger` is
available under `lpy.core`.

## Backwards compatibility

The `ledger` module is still provided as a thin shim:

```python
# python/ledger/__init__.py
from lpy.core import *
```

Existing scripts that use `import ledger` continue to work without any
changes.  However, the `ledger` shim is considered deprecated.  It will
not receive new names added to `lpy.core` in future releases unless the
shim is updated explicitly, and it does not expose any pure-Python
extensions added to `lpy/core/__init__.py`.

## Step-by-step migration

1. Replace the import line at the top of each script:

   ```bash
   # Automated rename for simple scripts
   sed -i 's/^import ledger$/from lpy import core/' myscript.py
   sed -i 's/\bledger\./core./g' myscript.py
   ```

2. If your script uses `from ledger import ...`, change it to:

   ```python
   from lpy.core import Amount, Balance, Journal  # etc.
   ```

3. Run your script to verify.  The behaviour of every type and function
   is identical — only the import path changed.

## Benefits of the new approach

### Pure-Python extensions alongside native types

Because `lpy/core/__init__.py` is an ordinary Python file that wraps
`_core`, it can define additional helpers or override names before they
are exported.  For example, you could add a convenience method to
`Amount` that formats values for a specific locale — without touching
any C++ code:

```python
# lpy/core/__init__.py  (simplified)
from ._core import *
from ._core import Amount as _NativeAmount

class Amount(_NativeAmount):
    """Amount with an extra helper for locale-aware formatting."""

    def display(self, locale="en_US"):
        import locale as _loc
        _loc.setlocale(_loc.LC_ALL, locale)
        return _loc.currency(self.to_double(), grouping=True)
```

Users then get the extended class transparently:

```python
from lpy import core

a = core.Amount("$1234.56")
print(a.display())   # $1,234.56
```

### Fixing issues in native types without rebuilding

The same mechanism allows you to intercept a buggy or missing method at
the Python layer rather than waiting for a C++ fix:

```python
# lpy/core/__init__.py
from ._core import *
from ._core import Journal as _NativeJournal

class Journal(_NativeJournal):
    def read_string(self, text):
        """Convenience wrapper missing from the C++ binding."""
        from lpy.core import read_journal_from_string
        return read_journal_from_string(text)
```

### Namespace room to grow

The `lpy` top-level package can host additional pure-Python sub-modules
without polluting the core namespace:

```python
from lpy import core        # native types
# future additions, e.g.:
# from lpy import reports   # high-level report helpers
# from lpy import convert   # importers / exporters
```

None of these additions require changes to the C++ layer.

## Using `lpy` in embedded mode (`ledger python`)

When a script is run via `ledger python myscript.py`, the embedded
Python interpreter automatically bootstraps both `lpy.core` and the
`ledger` shim in `sys.modules`, so neither `PYTHONPATH` nor an
installed package is required:

```bash
ledger python myscript.py   # both "from lpy import core" and
                             # "import ledger" work out of the box
```
