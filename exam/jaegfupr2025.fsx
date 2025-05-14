(*
I hereby declare that I myself have created this exam hand–in in its entirety without help from AI
tooling and anybody else.
*)
(*---------------------------------------------Question 1---------------------------------------------*)
type stack<'b> = list<'b>
type mapStack<'a,'b> = MapStack of list<'a*stack<'b>>

let ex1 = MapStack [('1',[3;1;1]); ('2',[1;3;1]); ('3',[1;1;3]);
    ('4',[3;3;1]); ('5',[2;3;4]); ('6',[2;1;2])]
//1.1
let ex2 =
    MapStack [
        ("Hey", ['H';'e';'y']);
        ("there", ['t'; 'h'; 'e';'r';'e']);
        ("", [])
    ]
(*
The type of ex2 is mapStack<string, char> because the tuples consist of a string and a list of chars.
The type declaration of MapStack specifies that the 'a type is the first value in the tuple and the 'b
type is what is in the stack (or list)*)

let empty<'a, 'b> () :mapStack<'a, 'b> = MapStack List.empty

//uses List.length function to count keys. Since each element in outer list is one key
let numKeys (MapStack m) =
    List.length m

//uses tail-recursive helper func with an accumulator to add length of each stack to acc and return that
let numValues (MapStack m) =
    let rec numValuesA (MapStack m) acc =
        match m with
        |[] -> acc
        |x::xs ->
            match x with
            |(key, vList )->
                numValuesA (MapStack xs) (acc+(List.length vList))
    numValuesA (MapStack m) 0


//1.2
//uses .Net library method to get head and tail and return them in a tuple.
let pop s =
    if List.length s =0 then failwith "Can't pop empty stack" else
    ((List.head s), (List.tail s))

//uses tail-recursive function with accumulator to check if key matches k and adds it to
//acc if it does not. Returns reverse accumulated list
let remove k l =
    let rec removeA k l acc =
        match l with
        |[] -> List.rev acc
        |(key, v)::xs ->
            if k=key then removeA k xs acc else removeA k xs ((key,v)::acc)
    removeA k l []
(* 
The "when 'a: equality" constraint in the type means that the argument given for k
and the first element in the tuple in the list given as argument for l must be comparable.
This is because they need to be compared to make decision to remove them or not
*)

//helper function to check if key exists in map
let rec mapExists k (MapStack m) =
    match m with
    |[] -> true
    |(key,v)::xs -> if k=key then true else mapExists k (MapStack xs)


//Uses tail-recursive helper function to recursively iterate over map and change what is added
//to the accumulator depending on if key matches k. 
let mapPush k v m =
    let rec mapPushA k v (MapStack m) acc=
        if not (mapExists k (MapStack m)) then (MapStack ((k, [v])::m)) else 
        match m with 
        |[] -> MapStack (List.rev acc)
        |(key, vList )::xs->
            if key=k then mapPushA k v (MapStack xs) ((k, ([v]@vList))::acc)
            else mapPushA k v (MapStack xs) ((key, vList)::acc)
    mapPushA k v m []


//recursively iterates over m to find key that matches k and uses remove and pop to return
//result. Uses helper function mapExists to check whether m contains k
//currently only returns map of keys with popped stacks. Explanation in in-line comment in function
let mapPop k (MapStack m) =
    if mapExists k (MapStack m) then
        let rec mapPopA k (MapStack m) acc  =
            match m with 
            |[] -> MapStack(List.rev acc)
            |(key, vList )::xs->
                if k=key then 
                    if (List.isEmpty vList) then failwith "Can't pop empty stack" 
                    else
                        match pop vList with
                        |(x, y) ->
                            mapPopA k (MapStack xs) (((List.head vList), y)::acc)
                else mapPopA k (MapStack xs) (acc) //should cons un-popped result into acc but that breaks type inference. Would be ((key, vList)::acc) instead of acc
        mapPopA k (MapStack m) []
    else failwith "Can't find key"


(* //test cases (commented out to not throw exceptions)
//tests regular functionality
mapPop '3' ex1

//tests popping from map that doesn't contain k
mapPop '7' ex1

//tests popping from empty stack
mapPop "" ex2 *)


//1.3
//uses tail-recursive accumulator helper function to recursively iterate over MapStack
//and List.map to map f on all values in the stack
let map f m=
    let rec mapA f (MapStack m) acc=
        match m with
        |[] -> MapStack(List.rev acc)
        |(k, v)::xs ->
            mapA f (MapStack xs) ((k, (List.map (fun x -> f k x) v))::acc)
    mapA f m []



(*Attempts at fold:*)

(* (*Tried making function with accumulator to keep entire result in *)
let fold f e m =
    let rec foldA f e (MapStack m) acc =
        match m with
        |[] -> List.rev acc
        |(k, v)::xs ->
            foldA f ((List.fold (fun acc x -> f k x) e v)::acc) (MapStack xs)
    foldA f e m [] *)

//Uses List.fold to apply f on each stack
let rec fold f e (MapStack m) =
        match m with
        |[] -> e
        |(k, v)::xs ->
            e+fold f ((List.fold (fun x -> f k x) e v)) (MapStack xs)

fold (fun str _ v -> str + v.ToString()) "" ex2





(*---------------------------------------------Question 2---------------------------------------------*)

//2.1
let log s = printfn "--> %s" s
let clean() = log "cleaning..."
let build() = log "building..."
let unitTest() = log "unit testing..."
let releaseTest() = log "release testing..."
let deploy() = log "deploying..."

//uses log function to print to console
let cpFile src dst () =
    log $"--> copying file from {src} to {dst} ..."

//The below statement returns an action that will log the copying from "foo.txt" to "Bar/"
cpFile "foo.txt" "Bar/" ()

//inaccurate function declaration which will break some later functions
let chain (a1: unit -> unit) (a2: unit->unit) ()  =
    a1 ()|> a2 

//broken because of chain
//let allTest = chain unitTest releaseTest

//declares (++) as global infix operator
let (++) = chain 
let allTest  = unitTest ++ releaseTest
allTest ()

//2.2
type buildStep = {
    name: string;
    action: unit -> unit;
    deps: string list
    }

//makes a record type with the arguments    
let mkBuildStep name action =
    {
        name= name;
        action= action;
        deps = []
    }

let cleanBS = mkBuildStep "clean" clean
let buildBS = mkBuildStep "build" build


let dependsOn name {name=n; action=a; deps=xs} =
    {
        name=n;
        action = a;
        deps =name::xs
    }

let buildBS' = dependsOn "clean" buildBS


//making record type with empty deps list and piping it to dependsOn twice with clean and build respectively
let testBS = 
    {
        name="test";
        action=allTest;
        deps =[]
    } |> dependsOn "clean" |> dependsOn "build"

//2.3
type buildStepEnv = Map<string,buildStep>
let bsEnv = Map.ofList [("clean",cleanBS);
    ("build",buildBS');
    ("test", testBS)]

//Pattern matches on Map.tryFind to either return build step or throw exception
let lookupBS name env =
    match Map.tryFind name env with
    |None -> failwith "Faild to find build step"
    |Some x -> x

//Pattern matches on Map.tryFind to find the buildStep and then deconstruct it in the pattern match
//to be able to return its dependencies (deps).
//Could have used the above function lookupBS instead of pattern matching again
let getDeps name env =
    match Map.tryFind name env with
    |None -> failwith "Failed to find build step"
    |Some {name=n; action=a; deps=xs} -> xs



//uses helper function with extra argument acc that keeps track of buildsteps that 
//have already been executed. 
//finds the buildsteps for dependencies by pattern matching on the list containing deps
//and using lookupBS to get the buildStep for that name. 
let exec name env = 
    let bs = lookupBS name env 
    let rec execA bs acc =
        match bs with
        |{name=n; action =a; deps =xs}->
            match xs with
            |[] ->()
            |x::xs ->
                if not (List.contains x acc) then 
                    match (lookupBS x env) with
                    |{name=n1; action =a1; deps =xs1} ->
                        a1()
                        execA bs (x::acc)
                else 
                    execA bs acc     
    execA bs []




(*---------------------------------------------Question 3---------------------------------------------*)
//3.1

let triangular n =
    if n<1 then failwith "Error, negative argument"
    else ((n*(n+1))/2)

//uses if statement in anonymous function and Seq.tail as a workaround to the exception on first element 0
let triangularSeq = Seq.initInfinite (fun i -> if i=0 then 1 else triangular i) |>Seq.tail

//uses Seq.cache to cache triangularSeq
let triangularSeq' = Seq.cache triangularSeq



//3.2
let catMapSlow n =
  let cache = Map.empty
  if n < 0 then failwith "Error, negative argument"
  else
    let rec cat n =
      if n = 0 then 1
      else
        if Map.containsKey n cache then cache[n]
        else
          let sum = List.fold (fun s i -> s + cat i * cat (n-1-i)) 0 [0..n-1]
          Map.add n sum cache   // <--- compiler warning
          sum
    cat n
catMapSlow 17

(*
catMapSlow is slow because the cache ends up being ignored by the compiler and 
therefore not used. This results in nothing being cached and no time being saved from
cheking whether a value already has been evaluated. *)


//initialises an array of length 10000000 consisting of 0's. The index in the array is n.
//To check whether a sum has been calculated before we check whether the element at index n in cache
//is not 0. If it's not 0, we know that is has been evaluated before and we can return that value
let catArray n =
  let mutable cache = Array.init 10000000 (fun i -> 0) 
  if n < 0 then failwith "Error, negative argument"
  else
    let rec cat n=
      if n = 0 then 1
      else
        if not (cache.[n]=0) then cache.[n]
        else
          let sum = List.fold (fun s i -> s + cat i * cat (n-1-i)) 0 [0..n-1]
          cache.[n] <- sum  
          sum
    cat n 

catArray 17 //returns 129644790

(*---------------------------------------------Question 4---------------------------------------------*)
//4.1

let rec countIf p = function
    [] -> 0
  | lst -> let (x::xs) = lst
           let count = countIf p xs
           if p x then count+1 else count

(*
The compiler warning stems from the fact that lst is pattern matched in the let statement
on it containing one or more elements. This can be fixed by pattern matching directly in the 
function statement as show below *)

let rec countIf2 p = function
    |[] -> 0
    |x::xs ->
        let count = countIf p xs
        if p x then count+1 else count


//4.2
//uses tail-recursive helper function with accumulator to pass the count as an argument 
//to the next recursive call
let countIfA p ls =
    let rec countIfAcc p ls acc =
        match ls with
        |[] -> acc
        |x::xs -> if p x then countIfAcc p xs (acc+1) else countIfAcc p xs acc
    countIfAcc p ls 0

//countIfA ((>)0) [-1; 2; -4; 5] //returns 2

//4.3
//uses extra argument acc in tail-recursive helper function and uses max function to 
//choose between largest argument between x and acc
let maxList xs =
    let rec maxListAcc ls acc =
        match ls with
        |[] -> List.head acc
        |x::xs ->
            if List.isEmpty acc then maxListAcc xs [x]
            else maxListAcc xs [(max x (List.head acc))]
    maxListAcc xs []
