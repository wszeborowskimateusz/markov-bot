open System.IO

let sampleFileName = "tarzan.txt"
let sample = 
    File.ReadAllLines sampleFileName
    |> String.concat " "


let GRAMS_LENGTH = 2

let bigramify (text:string) = 
    text.Split ' '
    |> Seq.windowed 2
    |> Seq.toArray

let nextWords (bigrams: string [] seq) (word: string) =
    bigrams
    |> Seq.filter (fun bigram -> bigram.[0] = word)
    |> Seq.map (fun bigram -> bigram.[1])
    |> Seq.toArray


let getRandomElementFromList (array: 'a array) =  
  let random = System.Random() 
  array.[random.Next(array.Length)]

let generateWords (sample: string) (firstWord: string) (textMaxLength: int) =
    let bigrams = bigramify sample
    let rec appendWord (sequance: string list) =
        let possibleNextWords = nextWords bigrams sequance.[0]
        if possibleNextWords.Length = 0 || sequance.Length >= textMaxLength then
            sequance
            // Reverse the list see we append to the begining
            |> List.rev
            |> String.concat " "
        else 
            let nextWord = getRandomElementFromList possibleNextWords
            appendWord (nextWord :: sequance)
    appendWord [firstWord]

let rec main() = 
    printf "Type starting sequance of words - separate words with space. Type exit() to exit the program: "
    let startingSequanceLine = System.Console.ReadLine()
    if startingSequanceLine = "exit()" then ()
    else 
        let startingSequance = startingSequanceLine.Split(" ")
        if startingSequance.Length <> GRAMS_LENGTH - 1 then
            printf "INVALID LENGTH OF INITIAL SEQUNCE\n"
        else
            printf "Length of sequance: "
            let len = int (System.Console.ReadLine())
     
            [0 .. 10]
            |> List.iter 
                (fun i -> 
                    printf "----------------------- START OF SEQUANCE %i --------------------------------\n" i
                    printf "%s\n" (generateWords sample startingSequance.[0] len)
                    printf "----------------------- END OF SEQUANCE %i --------------------------------\n" i
                ) 
           
            main()
