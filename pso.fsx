(* Particle Swarm Optimization in F#
    This application is meant as an exercise to implement PSO in a functional programming language, namely F#

    See this link for the psuedocode of PSO: http://www.swarmintelligence.org/tutorials.php
    See this cheat sheet for F# basics: https://dungpa.github.io/fsharp-cheatsheet/
*)

// Learning factors
// -------------------------------------------------------------------------------------------------------
// - Research shows that c1 and c2 should generally be equal and suggest that the best value is '2'
let c1 = 2
let c2 = 2

// Global best
let globalBest = 9999999999999999.0

// Types
type Position = {x: float; y: float}
type Particle = {position: Position; velocity: float}
type Swarm = {particles: Particle []}
