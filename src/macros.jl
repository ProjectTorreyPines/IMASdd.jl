"""
Conditional macro system for @nospecializeinfer based on LocalPreferences

This allows users to enable/disable @nospecializeinfer at precompile time
by setting the preference: use_nospecializeinfer = true/false
"""

using Preferences

# Read preference at precompile time
const USE_NOSPECIALIZEINFER = @load_preference("use_nospecializeinfer", true)

# Display setting during precompilation
@info "IMASdd: @nospecializeinfer is $(USE_NOSPECIALIZEINFER ? "ENABLED" : "DISABLED")"

"""
    @maybe_nospecializeinfer function_definition

Conditionally applies `@nospecializeinfer` based on `LocalPreferences.toml` setting.

# Configuration
Create `LocalPreferences.toml` next to `Project.toml`:
```toml
[IMASdd]
use_nospecializeinfer = false  # true (default) or false
```

Restart Julia after changes. See `CONFIGURATION.md` for details.
"""
macro maybe_nospecializeinfer(expr)
    if USE_NOSPECIALIZEINFER
        # Apply @nospecializeinfer
        return esc(:(Base.@nospecializeinfer $expr))
    else
        # Return function definition as-is
        return esc(expr)
    end
end

export @maybe_nospecializeinfer
