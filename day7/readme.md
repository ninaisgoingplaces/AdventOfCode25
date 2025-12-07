The second part of day 7 was an ideal problem 
for using Haskell's `List` monad for probabilistic 
computation. This way we can easily simulate all
possible paths in the *Many-World-Interpretation*
of Quantum Mechanics.

Unfortunately the number of possible worlds quickly
blows up (exponentially)! I am using a cheat code 
in that way, that every number of steps ($\Delta t$) 
I am letting the wavefunction collapse and 
messaure all endpoints of the many paths. 
Counting the number of occurences of a specific
endpoint in the set of possible outcomes gives
the probability for this endpoint. 

I can now continue in the QM way from these new
starting points and weigh the eventual outcome
of the new endpoints with the probability of
the starting points. This will yield the correct 
result.

I did measurements of the overall running 
times $T$ depending on the meassurement interval
$\Delta t$:


| $\Delta t$   |  $T$ |
|-------------------|---------|
| 20                |  35.20s |
| 30                |   7.90s |
| 40                |   7.06s |
| 50                |  20.10s |

There are surely more efficient algorithms 
for this problem, but I was pleasantly
suprised, that I could fix my exponential List-Monad algorithm
by introducing intermediate observations.
This somehow neatly ties back to a physics 
interpretation!