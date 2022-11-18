#add compendium
rcompendium::add_compendium(compendium = ".")

#add DESCRIPTION file (need to edit file by hand)
rcompendium::add_description() #prb


#create directories
dir.create("R")


#install all dependencies
remotes::install_deps(upgrade = "never")



#to be able to use pipes in functions
usethis::use_pipe()


#update NAMESPACE and add .Rd file for each function in man folder
devtools::document()

#load all functions
devtools::load_all()


#github tips
#command to stage files for git commit
git add -A
ls -al ~/.ssh

#uncommit
git reset --soft HEAD~1
