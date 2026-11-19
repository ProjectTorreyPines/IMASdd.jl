using IMASdd
using Test
import IMASdd.JSON as JSON

numpy(key; primitive="float64", parameters="{}") =
    """{"class": "NumpyArray", "primitive": "$primitive", "form_key": "$key", "parameters": $parameters}"""
list_offset(key, content; parameters="{}") =
    """{"class": "ListOffsetArray", "offsets": "i64", "content": $content, "form_key": "$key", "parameters": $parameters}"""
regular(key, content, size) =
    """{"class": "RegularArray", "size": $size, "content": $content, "form_key": "$key"}"""
byte_masked(key, content; valid_when=true) =
    """{"class": "ByteMaskedArray", "mask": "i8", "valid_when": $valid_when, "content": $content, "form_key": "$key"}"""

parse_form(form) = JSON.parse(form; dicttype=Dict{String,Any})

# (name, form, length, buffers, expected), buffers as ak.to_buffers names them
const AWKWARD_CASES = [
    ("var_float64", list_offset("node0", numpy("node1")), 3,
        Dict("node0-offsets" => [0, 3, 4, 4], "node1-data" => [1.0, 2.0, 3.0, 4.0]),
        [[1.0, 2.0, 3.0], [4.0], Float64[]]),
    ("var_option_float32", list_offset("node0", byte_masked("node1", numpy("node2"; primitive="float32"))), 2,
        Dict("node0-offsets" => [0, 2, 4], "node1-mask" => Int8[1, 0, 1, 1], "node2-data" => Float32[1, 2, 3, 4]),
        [Float32[1, NaN], Float32[3, 4]]),
    ("var_option_float32_valid_when_false",
        list_offset("node0", byte_masked("node1", numpy("node2"; primitive="float32"); valid_when=false)), 2,
        Dict("node0-offsets" => [0, 2, 4], "node1-mask" => Int8[0, 1, 0, 0], "node2-data" => Float32[1, 2, 3, 4]),
        [Float32[1, NaN], Float32[3, 4]]),
    ("var_var_float64", list_offset("node0", list_offset("node1", numpy("node2"))), 2,
        Dict("node0-offsets" => [0, 2, 3], "node1-offsets" => [0, 1, 3, 3], "node2-data" => [1.0, 2.0, 3.0]),
        [[[1.0], [2.0, 3.0]], [Float64[]]]),
    ("regular_var_var_float64", regular("node0", list_offset("node1", list_offset("node2", numpy("node3"))), 1), 1,
        Dict("node1-offsets" => [0, 2], "node2-offsets" => [0, 1, 3], "node3-data" => [1.0, 2.0, 3.0]),
        [[[[1.0], [2.0, 3.0]]]]),
    ("regular_float64", regular("node0", numpy("node1"), 3), 2,
        Dict("node1-data" => [0.0, 1.0, 2.0, 3.0, 4.0, 5.0]),
        [[0.0, 1.0, 2.0], [3.0, 4.0, 5.0]]),
    ("var_int64", list_offset("node0", numpy("node1"; primitive="int64")), 3,
        Dict("node0-offsets" => [0, 1, 3, 3], "node1-data" => [0, 1, 2]),
        [[0], [1, 2], Int[]]),
    ("var_string",
        list_offset("node0", list_offset("node1", numpy("node2"; primitive="uint8", parameters="""{"__array__": "char"}""");
            parameters="""{"__array__": "string"}""")), 2,
        Dict("node0-offsets" => [0, 2, 3], "node1-offsets" => [0, 6, 14, 17], "node2-data" => collect(codeunits("ECOILAECOILA_1F1A"))),
        [["ECOILA", "ECOILA_1"], ["F1A"]]),
    ("flat_float64", numpy("node0"), 3,
        Dict("node0-data" => [1.0, 2.0, 3.0]),
        [1.0, 2.0, 3.0]),
]

# (name, form, length, buffers, exception)
const AWKWARD_ERRORS = [
    ("unsupported_class", """{"class": "IndexedOptionArray", "index": "i64", "content": $(numpy("node1")), "form_key": "node0"}""", 1,
        Dict("node0-index" => [0], "node1-data" => [1.0]), ErrorException),
    ("masked_int", byte_masked("node0", numpy("node1"; primitive="int64")), 1,
        Dict("node0-mask" => Int8[1], "node1-data" => [1]), ErrorException),
    ("inner_shape", """{"class": "NumpyArray", "primitive": "float64", "inner_shape": [2], "form_key": "node0"}""", 1,
        Dict("node0-data" => [1.0, 2.0]), ErrorException),
    ("length_mismatch", numpy("node0"), 4,
        Dict("node0-data" => [1.0, 2.0, 3.0]), AssertionError),
]

@testset "awkward_from_buffers" begin
    for (name, form, len, buffers, expected) in AWKWARD_CASES
        @testset "$name" begin
            array = IMASdd.awkward_from_buffers(parse_form(form), len, buffers)
            @test isequal(array, expected)
        end
    end

    for (name, form, len, buffers, exception) in AWKWARD_ERRORS
        @testset "$name" begin
            @test_throws exception IMASdd.awkward_from_buffers(parse_form(form), len, buffers)
        end
    end

    @testset "views share the buffers" begin
        buffers = Dict("node0-offsets" => [0, 2, 3], "node1-data" => [1.0, 2.0, 3.0])
        array = IMASdd.awkward_from_buffers(parse_form(list_offset("node0", numpy("node1"))), 2, buffers)
        @test parent(array[1]) === buffers["node1-data"]
    end
end
