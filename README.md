# american-elections

How to connect RStudio to GitHub:
  1. Edit "main.R"
  2. On "Git" section (upper right) a new "main.R" will appear (if it's not there it means you didn't change anything from the previous script)
  3. Install usethis package and the in the console input: usethis::use_git_config(user.name = "your_name", user.email = "your_mail")
  4. Select this new "main.R" (point 3) and commit it
  5. After committing the file you need to make the change effective with "Git push"
  6. Git push (green ↑ arrow) it R will ask you your Github account username and then a password
  7. The password is a token you generated (imho 6 months validity should be good) from here "https://github.com/settings/tokens"
  8. I selected all "repo", but choose for yourself what use might be most suitable for you 
