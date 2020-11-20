open System.IO

let gramsLength = 4
let generalPrefixLength = gramsLength - 1
let sampleFileName = "tarzan.txt"
let sentenceFinishedMarker = "<SENTENCED_FINISHED>"


let getSampleTextFromFile =
    File.ReadAllLines sampleFileName
    |> String.concat " "


let parseInputText (text: string) =
    text.Split [| ' '; '"'; ',' |]
    |> Array.filter (fun s -> s <> "")
    |> Array.map (fun s ->
        if not (s.EndsWith '.') then
            [| s |]
        else
            [| s.[..(s.Length - 2)]
               sentenceFinishedMarker |])
    |> Array.reduce Array.append


let gramify (text: string) (inputParser: string -> string []) =
    text
    |> inputParser
    |> Seq.windowed gramsLength
    |> Seq.toArray


let doesNGramMatchWords (ngram: string []) (words: string []) =
    let differentBeginings =
        ngram
        |> Array.take words.Length
        |> Array.zip words
        |> Array.filter (fun pair -> fst pair <> snd pair)

    differentBeginings.Length = 0


let nextWords (ngrams: string [] seq) (initialWords: string []) =
    ngrams
    |> Seq.filter (fun bigram -> doesNGramMatchWords bigram initialWords)
    |> Seq.map (fun bigram -> bigram.[initialWords.Length])
    |> Seq.toArray


let takeLastNElements n array =
    array |> Array.rev |> Array.take n |> Array.rev


let getRandomElementFromList (array: 'a array) =
    let random = System.Random()
    array.[random.Next(array.Length)]


let generateWords (ngrams: string [] []) (firstWords: string []) (textMaxLength: int) =
    let rec appendWord (sequence: string []) =
        let possibleNextWords =
            nextWords ngrams (takeLastNElements generalPrefixLength sequence)

        if possibleNextWords.Length = 0
           || sequence.Length >= textMaxLength then
            String.concat " " sequence

        else
            let nextWord =
                getRandomElementFromList possibleNextWords

            if nextWord = sentenceFinishedMarker then
                (String.concat " " sequence)
                + sentenceFinishedMarker
            else
                appendWord (Array.append sequence [| nextWord |])

    appendWord firstWords


let print10Sequences ngrams startingSequence len =
    [ 0 .. 10 ]
    |> List.iter (fun i ->
        printf "----------------------- START OF SEQUANCE %i --------------------------------\n" i
        printf "%s\n" (generateWords ngrams startingSequence len)
        printf "----------------------- END OF SEQUANCE %i --------------------------------\n" i)

let fillNotFullInitialSequence ngrams (initialSequence: string[]) =
    let rec addNextWord (sequence: string[]) =
        let possibleNextWords = 
            (nextWords ngrams sequence)
            |> Array.filter (fun x -> x <> sentenceFinishedMarker)
        if sequence.Length = generalPrefixLength || possibleNextWords.Length = 0 then
            sequence
        else 
            let nextWord = getRandomElementFromList possibleNextWords
            addNextWord (Array.append sequence [|nextWord|])

    addNextWord initialSequence

let ngrams =
    gramify getSampleTextFromFile parseInputText

let rec main () =
    printf "Type starting sequance of words - separate words with space. Type exit() to exit the program: "
    let startingSequanceLine = System.Console.ReadLine()
    if startingSequanceLine = "exit()" then
        ()
    else
        let startingSequence = startingSequanceLine.Split(" ")
        printf "Length of sequance: "
        let len = int (System.Console.ReadLine())
        if startingSequence.Length < generalPrefixLength then
            printf "Length of sequence is too short. Will try to lengthen it.\n"
            let filledSequence = fillNotFullInitialSequence ngrams startingSequence
            printf "New sequence: %A\n" filledSequence
            print10Sequences ngrams filledSequence len
        else if startingSequence.Length > generalPrefixLength then
            printf "Sequence is too long taking last %d elements\n" generalPrefixLength
            let shortenedSequence = takeLastNElements generalPrefixLength startingSequence
            print10Sequences ngrams shortenedSequence len
        else
            print10Sequences ngrams startingSequence len

        main ()

main ()

(*
Autor: Mateusz Wszeborowski 165562

Zrealizowane zadania:
- Implementacja n-gramów - parametr gramsLength
- Elastyczne parsowanie tekstu - przykładowa implementacja parsera - parseInputText
- Inteligentne zakończenie zdań - meta słowo ze zmiennej sentenceFinishedMarker, które kończy sekwencję
- Możliwość podania krótszych sekwencji wejściowych - podajemy po prostu krótszą sekwencję


Działanie programu:
- Przed uruchomieniem należy wybrać interesujący plik z tekstem oraz krotność n gramów
    - odpowiadają za to parametry sampleFileName oraz gramsLength
- Uruchamiamy program - może to zająć dłuższą chwilę - wyliczany jest zbiór n-gramów dla tekstu
- Program w pętli będzie pytać nas o sekwencję (słowa oddzielamy spacją) oraz maksymalną długość sekwencji
- Wpisanie exit() w sekwencji spowoduje wyjście z pętli
- Program wypisze nam 10 przykładowych sekwencji
*)
