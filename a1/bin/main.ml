open A1

(**creates a state using wordle-La and wordle-Ta files which are the secret word
   dictionaries and all valid guesses dictionary respectively*)
let state = Wordle.init "data/wordle-La.txt" "data/wordle-Ta.txt"

(**stores the secret word for the given state*)
let secret = Wordle.getSecret state

(**checks if user wants cheat mode to see secret, if user inputs 1, cheat mode
   is enabled, if user inputs 0, cheat mode is disabled, if any other input is
   entered, function prompts user again

   postCondition: cheat mode is enabled or disabled depending on user input, and
   the word is revealed if cheat mode is enabled*)
let rec cheatMode () =
  print_endline "Do you want to enable cheat mode? (1 for yes, 0 for no)";
  let cheat = read_line () in
  if cheat = "1" then
    print_endline ("Cheat mode enabled. The secret word is " ^ secret ^ ".")
  else if cheat = "0" then print_endline "Cheat mode disabled. Good luck!"
  else
    let () =
      print_endline "Invalid input. Please enter 1 for yes or 0 for no."
    in
    cheatMode ()

(**prints character c with a green background and black text on terminal**)
let printGreen c =
  ANSITerminal.print_string [ ANSITerminal.on_green; ANSITerminal.black ] c

(**prints character c with a yellow background and black text on terminal**)
let printYellow c =
  ANSITerminal.print_string [ ANSITerminal.on_yellow; ANSITerminal.black ] c

(**prints character c with a white background and black text on terminal**)
let printGrey c =
  ANSITerminal.print_string [ ANSITerminal.on_white; ANSITerminal.black ] c

(*prints the colors of each character using helper methods in Wordle,
  precondition: the_input is what the user entered for their guess, the_input
  must be 5 characters

  postcondition: every character in the_input is printed with the correct
  color*)
let printColors the_input =
  let colorList = Wordle.getFinalColorList the_input state in
  for i = 0 to 4 do
    if List.nth colorList i = "green" then
      printGreen (String.make 1 (String.get the_input i))
    else if List.nth colorList i = "yellow" then
      printYellow (String.make 1 (String.get the_input i))
    else printGrey (String.make 1 (String.get the_input i))
  done;
  print_newline ()

(**main game loop, user gets 6 tries. checks if user input is correct and if
   user has any tries left n is decremented with each incorrect guess but not
   when a guess is invalid

   preconditins: n is the amount of tries the user gets

   postconditions: when n = 0, the game is over and the secret word is revealed
   when n > 0 and user guesses correctly, the game ends and the user wins*)
let rec prompt_and_print n =
  (*user has no more tries left*)
  if n = 0 then
    print_endline
      ("You have no more tries left. The word was " ^ secret
     ^ ". Better luck next time!")
  else
    let () = print_string "> " in
    let the_input = read_line () in
    (* let () = print_endline ("You entered: " ^ the_input) in *)

    (*makes everything lowercase so the guess is not case sensitive*)
    let the_input = String.lowercase_ascii the_input in

    (*checks if it is a valid word & if too many characters*)
    if the_input = "quit" then () (*checks if the_input is of valid length*)
    else if not (Wordle.checkCharInput the_input) then
      let () = print_endline "Input must be 5 letters long." in
      prompt_and_print n (*checks if the_input is a valid word in dictionary*)
    else if not (Wordle.validInput the_input state) then
      let () = print_endline "Input is not a valid word." in
      prompt_and_print n (*checks if the_input is equal to the secret word*)
    else if Wordle.checkSecret the_input state then (
      printColors the_input;
      print_endline
        ("Correct! The word is " ^ secret ^ ". \nYou took "
        ^ string_of_int (6 - n + 1)
        ^ " tries."))
    else (
      printColors the_input;
      prompt_and_print (n - 1))

(**prints the instructions for the game and asks about cheat mode. initializes
   the game by calling prompt_and_print with 6 guesses**)
let () =
  print_endline
    "Welcome to OCaml Wordle! Dark Mode is preferred. You will have 6 tries to \n\
     guess the secret 5 letter word. Type 'quit' to exit the game. Good luck!  \n\
     Green means the character is in the correct position, yellow means the \n\
     character is in the word but in the wrong position, and white means the \n\
     character is not in the word. \n";

  (*asks about cheat mode*)
  cheatMode ();

  (*initiate with 6 guesses*)
  prompt_and_print 6
