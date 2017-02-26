namespace FSharp.Stats.Optimization


module GradientDescent =

    open Microsoft.FSharp.Math 

    // we define a small number that we be used to calculate numerical approximations to derivatives:
    let eps = System.Double.Epsilon ** (1.0 / 3.0)

    // The following function repeatedly applies the given function to the given initial value until the result stops changing:
    let rec fixedPoint f x =
        let f_x = f x
        if f_x = x then x else fixedPoint f f_x


    // The numerical approximation to the grad of a scalar field is built up from partial derivatives in each direction:
    let partialD f_xs f (xs : vector) i xi =
        xs.[i] <- xi + eps
        try (f xs - f_xs) / eps finally
        xs.[i] <- xi


    // The following function performs a single iteration of gradient descent by scaling the step size lambda by either 'a' or 'b' if the result increases or decreases the function being minimized, respectively:
    let descend a b f (f': _ -> vector) (lambda, xs, f_xs) =
        let xs_2 = xs - lambda * f' xs
        let f_xs_2 = f xs_2
        if f_xs_2 >= f_xs then
          a * lambda, xs, f_xs
        else
          b * lambda, xs_2, f_xs_2


    /// radient descent algorithm to minimize a given function and derivative
    let minimize f f' xs =
        let _, xs, _ = fixedPoint (descend 0.5 1.1 f f') (eps, xs, f xs)
        xs

    /// Computes a numerical approximation to the derivative of a function
    let grad f xs =
        Vector.mapi (partialD (f xs) f xs) xs
   




