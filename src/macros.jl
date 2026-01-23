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

"""
    @_typed_cache cache_const function_definition

Internal macro: generates a cached version of a function using a typed dictionary.
Splits into: fast path (inline lookup), impl (body), slow path (noinline caching).

# Constraints
- No keyword arguments
- No where clauses
- No return type annotation
- Cached value must not be `nothing` (used as sentinel)

# Example
```julia
const _TCACHE_MY_FUNC = ThreadSafeDict{Int, String}()

@_typed_cache _TCACHE_MY_FUNC function my_func(x::Int)
    return string(x)
end
# Generates: my_func (fast), my_func_impl (body), my_func_slow (cache)
```
"""
macro _typed_cache(cache_const, func_def)
    # 1. Parse function definition
    if !(func_def isa Expr) || (func_def.head !== :function && func_def.head !== :(=))
        error("@_typed_cache expects a function definition")
    end

    call_expr = func_def.args[1]
    body = func_def.args[2]

    # Check for unsupported syntax
    if call_expr isa Expr && call_expr.head == :where
        error("@typed_cache does not support 'where' clauses (use concrete types)")
    end
    if call_expr isa Expr && call_expr.head == :(::)
        error("@typed_cache does not support return type annotations")
    end

    func_name = call_expr.args[1]
    args = call_expr.args[2:end]

    # Check for keyword arguments
    if !isempty(args) && args[1] isa Expr && args[1].head == :parameters
        error("@typed_cache does not support keyword arguments")
    end

    # Extract argument names (e.g., x::Int -> x)
    arg_names = map(args) do arg
        if arg isa Expr && arg.head == :(::)
            return arg.args[1]
        else
            return arg
        end
    end

    # Key generation: Single arg -> value, Multiple args -> tuple
    key_expr = length(arg_names) == 1 ? arg_names[1] : Expr(:tuple, arg_names...)

    # Generated function names
    impl_name = Symbol(func_name, :_impl)
    slow_name = Symbol(func_name, :_slow)

    # Gensym for hygiene (avoid collision with user's variable names)
    _key = gensym("key")
    _val = gensym("val")
    _result = gensym("result")

    return esc(quote
        # 1. Fast path - inline, single lookup
        Base.@inline function $func_name($(args...))
            $_key = $key_expr
            $_result = Base.get($cache_const, $_key, nothing)
            $_result !== nothing && return $_result
            return $slow_name($(arg_names...))
        end

        # 2. Impl - body execution (return-safe: returns from impl, not slow)
        function $impl_name($(args...))
            $body
        end

        # 3. Slow path - noinline, caching
        Base.@noinline function $slow_name($(args...))
            $_key = $key_expr
            $_val = $impl_name($(arg_names...))
            $cache_const[$_key] = $_val
            return $_val
        end
    end)
end

export @maybe_nospecializeinfer
