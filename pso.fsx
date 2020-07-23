(* Particle Swarm Optimization in F#
    This application is meant as an exercise to implement PSO in a functional programming language, namely F#

    See this link for the psuedocode of PSO: http://www.swarmintelligence.org/tutorials.php
    See this cheat sheet for F# basics: https://dungpa.github.io/fsharp-cheatsheet/
*)

// Utilities
let random = System.Random()

// Simulation
let maxiumumIterations = 100

// Learning factors
// -------------------------------------------------------------------------------------------------------
// - Research shows that c1 and c2 should generally be equal and suggest that the best value is '2'
let c1 = 2.0
let c2 = 2.0

// Types
type Position = {x: float; y: float; fitness: float}
type Particle = {position: Position; xVelocity: float; yVelocity: float; personalBest : Position}
type Swarm = {particles: list<Particle>; globalBest: Particle}

// Global best
let globalBest = 
    { position = {x = -1.0; y = -1.0; fitness = 9999999999.0}; xVelocity = random.NextDouble(); yVelocity = random.NextDouble(); personalBest = {x = -1.0; y = -1.0; fitness = 99999999.0}}

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

            { particle with xVelocity = presentVelocityTerm + personalBestTerm + globalBestTerm}

        let updateYVelocity (particle : Particle) =
            let presentVelocityTerm = 
                particle.yVelocity
            let personalBestTerm = 
                c1 * random.NextDouble() * (particle.personalBest.y - particle.position.y)
            let globalBestTerm = 
                c2 * random.NextDouble() * (swarm.globalBest.position.y - particle.position.y)

            { particle with yVelocity = presentVelocityTerm + personalBestTerm + globalBestTerm}

        let update =
            particle
            |> updateXVelocity
            |> updateYVelocity

        update particle

    if particles.Length = 0 then
        updatedParticles
    else
        //Why is this saying that
        updateParticleVelocities particles.Tail updatedParticles::(updateVelocities particles.Head) swarm

// Implement bounds check when I have the field implemented    
let rec updatePositions (particles : list<Particle>, updatedParticles : list<Particle>, swarm : Swarm) = 

    // Define the position update function
    let updatePosition (particle : Particle) =
        particle.position.x <| particle.position.x + particle.xVelocity
        particle.position.y <| particle.position.y + particle.yVelocity

    if particles.Length = 0 then
        updatedParticles
    else
        updatePositions particles.Tail updatedParticles::(updatePosition particles.Head) swarm

    
let initializeParticles(particles : list<Particle>, initializedParticles : list<Particle>, swarm : Swarm) =
    //This is a stub
    swarm

let findGBest(particles : list<Particle>, currentBest : Particle) =

    let currentParticle = particles.Head
    
    if currentParticle.personalBest.
