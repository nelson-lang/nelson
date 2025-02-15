import Pkg; Pkg.add("DifferentialEquations")
using DifferentialEquations

function lotka_volterra!(du, u, p, t)
    α, β, γ, δ = p
    du[1] = α * u[1] - β * u[1] * u[2]
    du[2] = δ * u[1] * u[2] - γ * u[2]
end

p = [0.1, 0.02, 0.3, 0.01]
u0 = [40.0, 9.0]
tspan = (0.0, 200.0)
prob = ODEProblem(lotka_volterra!, u0, tspan, p)
sol = solve(prob)

t = sol.t
prey = [sol.u[i][1] for i in 1:length(sol.u)] 
predators = [sol.u[i][2] for i in 1:length(sol.u)] 
