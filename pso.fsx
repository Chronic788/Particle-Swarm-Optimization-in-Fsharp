(* Particle Swarm Optimization in F#
    This application is meant as an exercise to implement PSO in a functional programming language, namely F#

    See this link for the psuedocode of PSO: http://www.swarmintelligence.org/tutorials.php
    See this cheat sheet for F# basics: https://dungpa.github.io/fsharp-cheatsheet/
*)

// Utilities
let random = System.Random()

// Learning factors
// -------------------------------------------------------------------------------------------------------
// - Research shows that c1 and c2 should generally be equal and suggest that the best value is '2'
let c1 = 2.0
let c2 = 2.0

// Global best
let globalBest = 9999999999999999.0

// Types
type Position = {x: float; y: float}
type Particle = {position: Position; xVelocity: float; yVelocity: float; personalBest : Position}
type Swarm = {particles: list<Particle>; globalBest: Particle}

let rec updateParticleVelocities (particles : list<Particle>, updatedParticles : list<Particle>, swarm : Swarm) =

    // Define the velocity update functions for the x and y axis
    let updateVelocities (particle : Particle) =

        let updateXVelocity (particle : Particle) =
            let presentVelocityTerm = 
                particle.xVelocity
            let personalBestTerm = 
                c1 * random.NextDouble() * (particle.personalBest.x - particle.position.x)
            let globalBestTerm = 
                c2 * random.NextDouble() * (swarm.globalBest.position.x - particle.position.x)

            //Is this how to update a value?
            particle.xVelocity <| presentVelocityTerm + personalBestTerm + globalBestTerm
            particle

        let updateYVelocity (particle : Particle) =
            let presentVelocityTerm = 
                particle.yVelocity
            let personalBestTerm = 
                c1 * random.NextDouble() * (particle.personalBest.y - particle.position.y)
            let globalBestTerm = 
                c2 * random.NextDouble() * (swarm.globalBest.position.y - particle.position.y)

            // Is this how to update a value??
            particle.yVelocity <| presentVelocityTerm + personalBestTerm + globalBestTerm
            particle

        let update =
            particle
            |> updateXVelocity
            |> updateYVelocity

        update particle

    if particles.Length = 0 then
        updatedParticles
    else
        updateParticleVelocities particles.Tail updatedParticles::(updateVelocities particles.Head) swarm
        

    
    
// Bounds check for velocity update
