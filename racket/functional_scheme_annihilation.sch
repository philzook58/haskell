(define (adag state)  newstate)
(define (a state)  newstate)

-- return a state -> state function given parameter k
(define (a_k k) (lambda (state) (newstate)))

-- could use vector monad to do passthrugh of amplitudes
-- then signature becomes occupation -> state

- could I also use bind for automatic conversion of bases . Somehow tag operators with possible basis they take in
-- then in bind incude all necessary basis conversion code (shortest path (most efficient) along conversion graph. Use A*)

--how to represent state?

--obvious solution. is essentially sparse vector representation 
(define examplestate (+  ket1 ket2 ket3))

(define ket1 (* amplitude (occupationlist)))

---


--would haskell help?
-- could I then define something like 
--   (a k) as:(a k):as = 0
--  (a k) a:as = if a = (a k) then 0  else if a = adag_k = else (-1)* 

-- keeping the ampltiude thunked also makes sense. e^ikx often will not have to be evaluated

-- conversion of a_k into a_x basis?

-- a(x) | 0> is actually (lambda x state)
-- currying? a(x) = state x -> state
-- hand a state and it becomes x-> state
-- flip curry and it becomes a concrete state->state
