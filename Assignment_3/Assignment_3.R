# Mikael Gouwtama
# 1007128127
# BTC1855 Assignment 3

#BM: overall, great job on this assignment
#your comments clearly explain the purpose of each code block
#your code is clear, logically sound, and well-written
#everything works smoothly with the code, and it performs
#its intended purpose
#i'm impressed with the way that you displayed the letters
#with blanks for the letters that were not guessed yet
#creative way to insert guess at the specific position in the word
#one improvement could be that user_input is detected as a word or letter
#automatically, instead of user inputting 1 or 2 to indicate their choice
#another improvement could be to screen words for special characters 
#and flag those as invalid inputs, instead of only screening 
#individual letters for special characters
#same thing with duplicate words guessed (screen for duplicate words,
#not only letters)
#otherwise, everything else is perfect with this assignment, great work


#BM: good use of readLines to make the txt.file a vector
#and sample for random word selection
#word list could have a larger selection of words

# Assuming that the words_list file is in the same directory as this file
# Get the list of words from the words_list.txt file and make it into a vector
words <- readLines("words_list.txt")

# Randomly select 1 word from the list of words
answer_key <- sample(words, 1)

# Splitting the answer key into a vector separating the individual letters would
# aid later in checking whether the user's guess is in the answer key 
answer_key <- unlist(strsplit(answer_key, ""))

#BM: great work on the clear explanantion for why separating individual letters
#is needed, you could have created a separate object to store the length
#of answer_key value if calling on it more than 1-2 times
#if you only use it 1-2 times, then this works well still

# Provide user the length of the answer
print(paste("The length of the answer word is", length(answer_key)))

# Initiate and report the number of tries the user has
# Here, we set the number of tries the user have as 5. 
tries <- 5
print(paste0("Guess this word. The number of tries you have is ", tries, ". Good Luck!"))

#BM: this is a creative way to display the hangman blanks to the user

# Create a display of the user's correct answer so far. This will begin as 
# underscores corresponding to the length of the answer key
user_answer <- rep("_", length(answer_key))

# Initialize an empty vector for the correct guesses
your_guesses <- vector() 

#BM: i like how you've split the user_input into a
#function with your_guesses, keeps the entire user input
#section separate from the main loop portion
#great use of nested if-else statements and repeat loops
#to execute this portion, everything works smoothly

# Get the user input
user_input <- function(your_guesses) {
  # Ask the user to choose whether they want to guess a letter or a word
  repeat {
    input_1 <- readline(prompt = "Guess a letter or a word? Choose 1 for letter, 2 for word: ")
    # If user correctly entered either 1 or 2
    if (input_1 == "1" | input_1 == "2") {
      # If the user choose to guess a letter, ask the user to enter the letter
      if (input_1 == "1") {
        repeat {
          input_2 <- readline(prompt = "Enter a single letter: ")
          # Based on the letter input, do the following
          if (input_2 %in% your_guesses) {
            cat("You have already guessed this. Please enter a different letter.\n")
          } else if (nchar(input_2) == 1 & grepl("^[a-zA-Z]$", input_2)) {
            # Return a list specifying "letter" and the input value if the user 
            # correctly enter a single letter
            # Make the values lower case so that letter cases difference are ignored
            # This is one way to exit the repeat loop
            return(list(input_type = "letter", input_value = tolower(input_2)))
          } else {
            cat("Input is invalid. Please enter a single letter.\n")
          }
        }
      } else if (input_1 == "2") {
        
#BM: smart thinking with conversion to lowercase here
        
        # If the user wants to enter a word, record the word after making them
        # all lowercase in a list with the type "word"
        # This is a second way to exit the repeat loop
        input_3 <- readline(prompt = "Enter a word: ")
        return(list(input_type = "word", input_value = tolower(input_3)))
      }

#BM: not an issue at all, but this else statement could have included 
#a message stating that their input was invald
#good work with reiterating that either 1 or 2 (and nothing else) needs to be entered in
      
    } else {
      # If the user incorrectly enters 1/2, ask it again and repeat the loop
      cat("Please input either 1 or 2. 1 for letter and 2 for word.\n")
    }
  }
}

#BM: good work with the while loop and progress update
#the code runs smoothly with no issues

# Main loop for the hangman
# A while loop is used such that the game keeps running until the user exhausted
# all 5 of tries
while (tries != 0) {
  # Show the user their current progress
  cat("Current progress: ", paste(user_answer, collapse = ""), "\n")
  # Show the user their guesses so far
  cat("Your guesses so far are: ", paste(your_guesses, collapse = ""), "\n")
  # Tell the user their remaining tries
  cat("Remaining tries: ", tries, "\n")

#BM: this is great use of the user_input function
#good work with progress updates for user as well

  # Get input from user by calling the user_input function
  user_input_result <- user_input(your_guesses)

#BM: this is an efficient way to isolate the different types in the list
  
  # Since the input is a list with the type and the values, we isolate them into
  # 2 variables, x and y
  x <- user_input_result$input_type
  y <- user_input_result$input_value

  #BM: this works very well, my only criticism is that input could have been
  #verified as a letter or word earlier with a logical operator, rather at this point
  #that way, the user would not have to enter 1 or 2 each time
  #code would automatically identify input as word, letter, or invalid
  
  # Looking at the input type, enter this if condition if the input is a letter
  if (x == "letter") {

#BM: smart thinking to store user guesses in a separate vector
    
    # Update the user's guesses so far with the input value
    your_guesses <- sort(c(your_guesses, y))

#BM: good use of %in%
  
    # Continue to check whether the input value (the letter) is in the answer key
    # If it is in the answer key, continue with this if conditional loop 
    if (y %in% answer_key) {
      cat("Nice, you have guessed a letter correctly!\n")

#BM: this was an efficient way to give the user visual cues of their progress
      #with the blanks and letters filled in
      #the way you went about it is perfect, amazing work here
      #it works smoothly with no issues that i found
      
      # Get the position of the guess
      positions <- which(y == answer_key)
      # Update the user answer (the underlines showing the word) with the correct
      # guess at its correct position
      user_answer[positions] <- y

  #BM: good use of identical here to match user_answer to the word
      
      # If the user has guessed the last letter such that the user's answer matches
      # the answer key, end the game and congratulate them
      if (identical(user_answer, answer_key)) {
        cat("Congratulations! You have guessed the word correctly:", 
            paste(answer_key, collapse = ""))
        break
      }
      
    # If the input value (the letter) is not in the answer key, deduct 1 trial
    } else {
      tries <- tries - 1
      cat("Oops, incorrect guess.\n")
    }

  #BM: this block for guessing the word works well too
    #code does not run infinitely, good work exiting the loop with break
  
  # if the user decide to guess the word directly, enter this else if condition
  } else if (x == "word") {
    # If they guessed the answer key correctly, end the game and congratulate them
    if (y == paste(answer_key, collapse = "")) {
      cat("Congratulations! You have guessed the word correctly:", 
          paste(answer_key, collapse = ""))
      break
    # If they guessed the word wrong, deduct 1 trial
    } else {
      tries <- tries - 1
      cat("Oops, incorrect guess.\n")
    }
  }
}

#BM: this ends the game and performs its purpose well
#works efficiently, exits loop appropriately

# if the user ran out of tries, tell them they lose as the main loop for the
# game will no longer run
if (tries == 0) {
  cat("You ran out of tries, you lose :(", "\n")
  cat("The correct answer is:", paste(answer_key, collapse = ""), "\n")
}
