var userChoice;
var computerChoice;

var choosing = function(user, comp) {
    user = prompt("Do you choose rock, paper or scissors?");
    userChoice = user;
    comp = Math.random();
    if (comp < 0.34) {
	    comp = "rock";
    } else if(comp <= 0.67) {
	    comp = "paper";
    } else {
	    comp = "scissors";
    } 
    computerChoice = comp;
    console.log("Human: " + userChoice)
    console.log("Computer: " + computerChoice);
};

var compare = function (choice1, choice2) {
    if (choice1 === choice2) {
        console.log("The result is a tie! REMATCH:");
        choosing(userChoice, computerChoice);
        return compare(userChoice, computerChoice);
    }
    else if (choice1 === "rock") {
        if (choice2 === "scissors") {
            return "rock wins";
        }
        else {
            return "paper wins";
        }
    }
    else if (choice1 === "paper") {
        if (choice2 === "rock") {
            return "paper wins";
        }
        else {
            return "scissors wins";
        }
    }
    else if (choice1 === "scissors") {
        if (choice2 === "rock") {
            return "rock wins";
        }
        else {
            return "scissors wins";
        }
    }
    else {
        console.log("You didn't choose anything close to right! I guess the computer wins ... you definitely lose");
    }
};

choosing(userChoice, computerChoice);
compare(userChoice, computerChoice);
