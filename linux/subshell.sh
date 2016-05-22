#!/bin/bash

export A="1"
B="2"

echo "A = ${A}, B = ${B}"

(
    export A="42"
    B="43"
    export C="44"
    D="45"

    # A, B, C and D get new values
    echo "A = ${A}, B = ${B}, C = ${C}, D = ${D}"
)

# A and B get old value, C and D are not defined here
echo "A = ${A}, B = ${B}, C = ${C}, D = ${D}"
