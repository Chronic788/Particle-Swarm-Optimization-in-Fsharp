(* Particle Swarm Optimization in F#
    This application is meant as an exercise to implement PSO in a functional programming language, namely F#

    See this link for the psuedocode of PSO: http://www.swarmintelligence.org/tutorials.php
    See this cheat sheet for F# basics: https://dungpa.github.io/fsharp-cheatsheet/
*)

// Utilities
let random = System.Random()

// Simulation
let maxiumumIterations = 100
let fieldWidth = 500.0
let fieldHeight = 500.0

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
let globalBest = { 
                  position = {x = -1.0; y = -1.0; fitness = 9999999999.0}; 
                  xVelocity = random.NextDouble(); 
                  yVelocity = random.NextDouble(); 
                  personalBest = {x = -1.0; y = -1.0; fitness = 99999999.0}}

let updateParticleVelocities (swarm : Swarm) =

    let particles = swarm.particles

    let updateVelocities (particle : Particle) =

        let updateXVelocity (particle : Particle) =
            let presentVelocityTerm = 
                particle.xVelocity
            let personalBestTerm = 
                c1 * random.NextDouble() * (particle.personalBest.x - particle.position.x)
            let globalBestTerm = 
                c2 * random.NextDouble() * (swarm.globalBest.position.x - particle.position.x)

            presentVelocityTerm + personalBestTerm + globalBestTerm

        let updateYVelocity (particle : Particle) =
            let presentVelocityTerm = 
                particle.yVelocity
            let personalBestTerm = 
                c1 * random.NextDouble() * (particle.personalBest.y - particle.position.y)
            let globalBestTerm = 
                c2 * random.NextDouble() * (swarm.globalBest.position.y - particle.position.y)

            presentVelocityTerm + personalBestTerm + globalBestTerm

        { particle with xVelocity = (updateXVelocity particle); yVelocity = (updateYVelocity particle)}

    { swarm with particles = List.map (fun particle -> updateVelocities particle) particles}

// Implement bounds check when I have the field implemented    
let rec updatePositions (swarm : Swarm) = 

    let particles = swarm.particles

    // Define the position update function
    let updatePosition (particle : Particle) =
        let position = particle.position
        { particle with position = {position with x = particle.position.x + particle.xVelocity; y = particle.position.y + particle.yVelocity}}

    { swarm with particles = List.map (fun particle -> updatePosition particle) particles}

    
let initializeParticles(swarm : Swarm, particleCount : int) =
    
    let newParticles = [
        for i in 1 .. particleCount do
            let randomPositon = {
                                 x = random.NextDouble() * fieldWidth;
                                 y = random.NextDouble() * fieldHeight;
                                 fitness = 9999999999.0;
                                }
            yield { 
                   position = randomPositon;
                   xVelocity = random.NextDouble();
                   yVelocity = random.NextDouble();
                   personalBest = randomPositon;
                  }
    ]

    { swarm with particles = newParticles }

let calculateFitnesses (swarm : Swarm) =
    printfn "I am a stub. Implement me when you implement the json of the simulation state."