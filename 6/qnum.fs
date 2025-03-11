let (+) a b =
    { first = a.first + b.first; second = a.second + b.second }

let (-) a b =
    { first = a.first - b.first; second = a.second - b.second }

let (*) a b =
    { first = a.first * b.first - a.second * b.second;
      second = a.first * b.second + a.second * b.first }

let (/) a b =
    let denominator = b.first ** 2.0 + b.second ** 2.0
    { first = (a.first * b.first + a.second * b.second) / denominator;
      second = (a.second * b.first - a.first * b.second) / denominator }