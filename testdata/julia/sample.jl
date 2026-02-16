using LinearAlgebra
using Statistics

export greet, compute, Point

struct Point
    x::Float64
    y::Float64
end

function greet(name::String)
    println("Hello, $name")
end

function compute(a::Vector{Float64}, b::Vector{Float64})
    return dot(a, b)
end

function _internal(x)
    return x * 2
end
