## Group members : 1.Duoer Chen 2.Yaqi Liu 3.Yitian Zhu
## Contributions : 1.Duoer Chen 30% 2.Yaqi Liu 35% 3.Yitian Zhu 35%
## Explanations : We roughly divided the work as follows:
## Duoer Chen was mainly responsible for the programming work before Q6,
## Yitian Zhu was responsible for Q7&Q8, Yaqi Liu was responsible for Q9&Q10.
## But throughout the work we spent a lot of time discussing and researching together,
## and constantly improving our data structures.

#Question2&3.
# download and read the text
url <- "https://www.gutenberg.org/files/4300/4300-0.txt"
download.file(url, destfile = "4300-0.txt", method = "curl")
a <- scan("4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73, fileEncoding = "UTF-8")
a <- gsub("_", "", a, fixed = TRUE) # remove _
a <- gsub("—", "", a, fixed = TRUE) # remove —

#Question4.
# define punct_split function
punct_split <- function(a) {
  i <- length(a)
  punct_index <- grep("[,.;!:?]", a) # find the punctuation
  ii <- length(punct_index) 
  a_split <- rep("", i + ii) # vector to store single word or punctuation
  iic <- punct_index + 1:ii # punctuation location
  a_split[iic] <- substr(a[punct_index], nchar(a[punct_index]), nchar(a[punct_index])) # store punctuation
  a_split[-iic] <- a # store words
  a_split[iic - 1] <- substr(a[punct_index], 1, nchar(a[punct_index]) - 1)
  return(a_split)  # return processed text
}

#Question5.
# separate the punctuation marks of a
a_punctsplit <- punct_split(a)

#Question6.
#(a)
a_lower <- tolower(a_punctsplit) # change to the lower case
a_unique <- unique(a_lower) # find the vector of unique words

#(b)
a_match <- match(a_lower, a_unique) # match text to unique words

#(c)
freq <- tabulate(a_match) # count up how many time each unique word occurs in the text

#(d)
freq_order <- order(freq, decreasing = TRUE)[1:1000] # find the 1000 common words

#(e)
b <- a_unique[freq_order] # 1000 common words vector

#Question7.
#(a)
mlag <- 4
index_b <- match(a_lower, b) # match the text with 1000 common words
n <- length(a_lower)

#(b)
# create Markov model matrix
M <- matrix(NA, nrow = n - mlag, ncol = mlag + 1)
for (i in 0:mlag) {
  M[, i + 1] <- index_b[(1 + i):(n - mlag + i)]
}


#Question8
nw <- 50
sentence_index <- list()
# choose an initial word at random
first_index <- sample(M[which(!is.na(M[, 1])),1], 1)
sentence_index <- append(sentence_index, first_index)
# generated text
for (i in 2:nw) {
  found_next <- FALSE
  # rollback from the higher-order model(mlag) to lower-order model
  for (j in mlag:1) {
    if (i > j) {
      window <- unlist(sentence_index[(i-j):(i-1)]) # get the current sequence of words
      matching_rows <- which(rowSums(M[, 1:j, drop = FALSE] == matrix(window, nrow = nrow(M), ncol = j, byrow = TRUE)) == j & !is.na(M[, j + 1])) # find the matching rows and make sure the next word is not NA.
      if (length(matching_rows) > 1) {
        random_next <- sample(M[matching_rows, j + 1], 1) # generate the next word form matched rows
        sentence_index <- append(sentence_index, random_next) # put generated word index into sentence
        found_next <- TRUE
        break  # break out  as soon as the next word has successfully been generated
      }
    }
  }
  
  # if the selected initial word cannot find the next word, the initial word is selected again.
  if (!found_next) {
    random_next <- sample(M[which(!is.na(M[, 1])),1], 1)
    sentence_index <- append(sentence_index, random_next)
  }
}
generated_sentence <- b[unlist(sentence_index)] #change index to real words
cat(generated_sentence, sep = " ") #print the sentence

#Question9
index_b <- match(a_lower, b)
freq_b <- tabulate(index_b)
freq_b
probs = freq_b/sum(freq_b) # calculate the probabilities
sum(probs) #=1
nw=50
simulate50 <- sample(b,nw,replace=T,prob=probs) # simulate the sentence
cat(simulate50)


# Question 10
# Download and read the text
url <- "https://www.gutenberg.org/files/4300/4300-0.txt"
download.file(url, destfile = "4300-0.txt", method = "curl")
a <- scan("4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73, fileEncoding = "UTF-8")

# Remove unnecessary characters
a <- gsub("_", "", a, fixed = TRUE)
a <- gsub("—", "", a, fixed = TRUE)
a <- gsub("_(", "", a, fixed = TRUE)

# Define punctuation splitting function
punct_split <- function(a) {
  i <- length(a)
  punct_index <- grep("[,.;!:?]", a)
  ii <- length(punct_index)
  a_split <- rep("", i + ii)
  iic <- punct_index + 1:ii
  a_split[iic] <- substr(a[punct_index], nchar(a[punct_index]), nchar(a[punct_index]))
  a_split[-iic] <- a
  a_split[iic - 1] <- substr(a[punct_index], 1, nchar(a[punct_index]) - 1)
  return(a_split)  # Return the processed text
}

# Split punctuation
a_punctsplit10 <- punct_split(a)

# Find unique words
a_unique10 <- unique(a_punctsplit10)

# Map the text to indices of unique words
a_match10 <- match(a_punctsplit10, a_unique10)

# Calculate word frequencies
freq <- tabulate(a_match10)

# Find the top 1000 most common words
freq_order10 <- order(freq, decreasing = TRUE)[1:1000]
b10 <- a_unique[freq_order10]

# Define the order of the Markov model
mlag <- 4

# Map the text to the indices of the top 1000 common words
index_b <- match(a_punctsplit, b)
n <- length(a_punctsplit)

# Create the Markov model matrix
M <- matrix(NA, nrow = n - mlag, ncol = mlag + 1)

for (i in 0:mlag) {
  M[, i + 1] <- index_b[(1 + i):(n - mlag + i)]
}

# Set the length of the generated text
nw <- 50
sentence_index <- list()

# Randomly choose an initial word index
first_index <- sample(M[which(!is.na(M[, 1])),1], 1)
sentence_index <- append(sentence_index, first_index)

# Start generating the text
for (i in 2:nw) {
  found_next <- FALSE
  
  # Backtrack from higher-order model (mlag) to lower-order
  for (j in mlag:1) {
    if (i > j) {
      # Get the sequence of words in the current window
      window <- unlist(sentence_index[(i-j):(i-1)])
      
      # Find matching rows and ensure the next word is not NA
      matching_rows <- which(rowSums(M[, 1:j, drop = FALSE] == matrix(window, nrow = nrow(M), ncol = j, byrow = TRUE)) == j & !is.na(M[, j + 1]))
      
      if (length(matching_rows) > 1) {
        # Randomly choose the next word from matching rows
        random_next <- sample(M[matching_rows, j + 1], 1)
        
        # Append the chosen word index to the sentence
        sentence_index <- append(sentence_index, random_next)
        found_next <- TRUE
        break  # Exit inner loop when the next word is found
      }
    }
  }
  
  # If no matching word is found (fallback to 0th-order model), randomly choose one
  if (!found_next) {
    random_next <- sample(M[which(!is.na(M[, 1])),1], 1)
    sentence_index <- append(sentence_index, random_next)
  }
}

# Convert the generated indices back to words
generated_sentence <- b[unlist(sentence_index)]

# Print the generated sentence
final_sentence <- cat(generated_sentence, sep = " ")

# Capture the output from cat
cat_output <- capture.output(cat(generated_sentence, sep = " "))

# Store the content as a string
final_string <- paste(cat_output, collapse = "\n")

final_string <- gsub(" ([,.;!:?])", "\\1", final_string)

# Print the final result
print(final_string)
