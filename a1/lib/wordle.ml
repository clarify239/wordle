(**reads lines from sFIleName and returns a string list
these are all possible guesses*)
let secWordList sFileName = BatList.of_enum (BatFile.lines_of sFileName)
(**reads lines from fileName and returns a stringList
these are all possible secret words*)
let wordList fileName = BatList.of_enum (BatFile.lines_of fileName)

(**Wordle game state containing secret_word and dictionary
secret_word should be a 5 character word and can be found in dictionary
dictionary contains all valid guesses*)
type state = {
  secret_word : string;
  dictionary : string list;
}

(**picks a random secret word from secWordList
secWordList contains all possible secret words
returns a random secret word from secWordList*)
let pickRandom secWordList =
  Random.self_init ();
  let n = List.length secWordList in
  let i = Random.int n in
  List.nth secWordList i

  (**parameterized so that any files can be chosen as parameters
  sFileName is the dictionary of all possble secret words
  fileName is the dictionary of all possible guesses
  returns a state with a secret word and dictionary*)
let init sFileName fileName : state =
  {
    secret_word = pickRandom (secWordList sFileName);
    dictionary = wordList fileName;
  }

(** getter so that secret word can be accessed in main.ml 
returns the secret word of the given state*)
let getSecret state = state.secret_word

(*checks if the_input is equal to secret, returns true if true and false otherwise*)
let checkSecret the_input state : bool =
  if the_input = getSecret state then true else false

(**checks if the_input is a valid word in dictionary, returns true if true and 
  false otherwise*)
let validInput the_input state : bool =
  (*only 5 letter words are allowed*)
  if not (List.mem the_input state.dictionary) then false else true

(**checks if the_input is 5 characters, returns true if true and false otherwise*)
let checkCharInput the_input : bool =
  if String.length the_input != 5 then false
    (*word must be a valid word in dictionary*)
  else true

(**removes first character of a string s and returns s*)
let removeFirst s = String.sub s 1 (String.length s - 1)

(**compares the_input and secret to create a boolList indicating whether there 
should be a green tile at the corresponding location
both the_input and secret are decremented with each recursive call

preconditions:
the_input is the user's guess, secret is the secret word, both must be 5 characters.
when makeGreenList is initially called boolList must be empty i.e. []

postconditions:
returns final boolList of whether each position should be green when 
the_input is empty*)
let rec makeGreenList (the_input : string) (secret : string)
    (boolList : bool list) : bool list =
  if String.length the_input = 0 then List.rev boolList
    (*must reverse because can only prepend instead of append to a list*)
  else if the_input.[0] = secret.[0] then
    makeGreenList (removeFirst the_input) (removeFirst secret) (true :: boolList)
  else
    makeGreenList (removeFirst the_input) (removeFirst secret)
      (false :: boolList)

(**creates a string of nongreen letters using information from makeGreenList
secret is the secret word, leftover is the string of nongreen letters so far, 
both must be 5 characters
secret is decremented with each recursive call, leftover is added to when 
there is a non-green tile

precondition:
when makeNoneGreen is initially called, leftover should always be empty i.e. ""
and boolList should be created using helper function makeGreenList

postcondition:
when secret is empty, leftover is returned and is the  string of nongreen letters 
*)

let rec makeNonGreen (secret : string) (leftover : string)
    (boolList : bool list) : string =
  if String.length secret = 0 then leftover
  else if List.hd boolList = false then
    makeNonGreen (removeFirst secret)
      (leftover ^ String.make 1 secret.[0])
      (List.tl boolList)
  else makeNonGreen (removeFirst secret) leftover (List.tl boolList)

(**creates a boolList of indicating whether there should be a yellow tile at 
each position using information from makeGreenList and makeNonGreen
the_input is the user's guess and leftover is non-used letters from secret
both are decremented with each recursive call

preconditions:
the_input and leftover must be 5 characters
when makeYellowList is initially called, leftover should be called using helper 
makeNonGreen, boolList should be empty, and greenList should be called with 
makeGreenList

postcondition:
returns final boolList of whether each position should be yellow when 
the_input is empty*)

let rec makeYellowList the_input leftover boolList greenList : bool list =
  if String.length the_input = 0 then List.rev boolList
    (*if leftover contains the_input.[0] and it's not ALREADY green*)
  else if String.contains leftover the_input.[0] && not (List.hd greenList) then
    let x = String.index leftover the_input.[0] in
    makeYellowList (removeFirst the_input)
      (String.sub leftover 0 x
      ^ String.sub leftover (x + 1) (String.length leftover - x - 1))
      (true :: boolList) (List.tl greenList)
  else
    makeYellowList (removeFirst the_input) leftover (false :: boolList)
      (List.tl greenList)

(**finally putting it all together, definitely not the most efficient way to do 
it but it works! creates a list of strings indicating the color of each tile
both greenList and yellowList are returned without their first element with 
each recursive call

preconditions: 
greenList and yellowList called using helpers makeGreenList and makeYellow 
respectively
when initially called, boolList should be empty

postcondition:
when greenList is empty, boolList will be the final list of colors and is returned
*)
let rec createFinalColorList greenList yellowList boolList : string list =
  if List.length greenList = 0 then List.rev boolList
  else if List.hd greenList then
    createFinalColorList (List.tl greenList) (List.tl yellowList)
      ("green" :: boolList)
  else if List.hd yellowList then
    createFinalColorList (List.tl greenList) (List.tl yellowList)
      ("yellow" :: boolList)
  else
    createFinalColorList (List.tl greenList) (List.tl yellowList)
      ("grey" :: boolList)

(**calls createFinalColorList but only has parameters the_input and state so 
that getFinalColorList can be easily called in the interface main.ml file 

preconditions:
the_input must be 5 characters long and be a valid word in the dictionary
state must be a valid game state with a secret word of 5 characters
*)
let getFinalColorList the_input state =
  let greenList = makeGreenList the_input (getSecret state) [] in
  let leftOver = makeNonGreen (getSecret state) "" greenList in
  let yellowList = makeYellowList the_input leftOver [] greenList in
  let colorList = createFinalColorList greenList yellowList [] in
  colorList
