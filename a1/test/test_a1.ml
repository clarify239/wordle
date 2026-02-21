open OUnit2
open A1.Wordle

let tests =
  "test suite for Wordle"
  >::: [
         (
          (**testing basic functions of Wordle such as if the word is in the 
            dictionary and if the guesses are valid**) 
          "basic testing" >:: fun _ ->
           (*smaller files for testing*)
           let s =
             init "../data/wordle-La-test.txt" "../data/wordle-Ta-test.txt"
           in
           assert_bool "secret word is in dictionary"
             (List.mem s.secret_word s.dictionary);

           (*secret word is 5 char*)
           assert_equal 5 (String.length s.secret_word);

           (*secret word is same as the getter*)
           assert_equal s.secret_word (getSecret s);

           (*test to see if correct dictionary is loaded (ta version instead of
             la)*)
           assert_bool "abaya is in dictionary" (List.mem "abaya" s.dictionary);

           assert_bool "checking checkSecret function"
             (checkSecret s.secret_word s);

           assert_bool "checking checkSecret function with wrong word"
             (not (checkSecret "aaaaa" s));

           assert_bool "validInput should return TRUE for valid word"
             (validInput "abaya" s);

           assert_bool "validInput should return FALSE for invalid word"
             (not (validInput "belch" s));

           assert_bool "checkCharInput should return FALSE for invalid length"
             (not (checkCharInput "beachy"));
           assert_bool "" (not (checkCharInput "beat"));

           assert_bool "checkCharInput should return TRUE for valid length"
             (checkCharInput "beach") );
         (*testing the makeGreenList function*)
         ( "test makeGreenList function" >:: fun _ ->
           assert_equal
             [ true; true; false; false; false ]
             (makeGreenList "abcde" "abbbb" []);

           assert_equal
             [ false; false; false; false; false ]
             (makeGreenList "abcde" "fghij" []);

           assert_equal
             [ true; true; true; true; true ]
             (makeGreenList "abcde" "abcde" []) );
         (**testing makeNonGreen function**)
         ( "test makeNonGreen function" >:: fun _ ->
           assert_equal "flp"
             (makeNonGreen "fbldp" "" [ false; true; false; true; false ]) );
         (**test getFinalList function**)
         ( "test getFinalColorList function" >:: fun _ ->
           let s =
             { secret_word = "apple"; dictionary = [ "apple"; "appel" ] }
           in
           assert_equal
             [ "green"; "green"; "green"; "yellow"; "yellow" ]
             (getFinalColorList "appel" s);

           assert_equal
             [ "yellow"; "yellow"; "green"; "yellow"; "yellow" ]
             (getFinalColorList "papel" s);

           assert_equal
             [ "grey"; "green"; "green"; "yellow"; "grey" ]
             (getFinalColorList "pppaz" s);

           assert_equal
             [ "grey"; "green"; "green"; "grey"; "grey" ]
             (getFinalColorList "ppppp" s);

           (*makes sure correct amount of yellows are shown*)
           assert_equal
             [ "yellow"; "yellow"; "grey"; "yellow"; "grey" ]
             (getFinalColorList "paapp" s) );
       ]

let _ = run_test_tt_main tests
