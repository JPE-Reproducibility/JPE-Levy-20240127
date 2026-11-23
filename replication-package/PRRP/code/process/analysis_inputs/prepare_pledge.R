file.create("code/pledgeConfig")
tryCatch(source("code/process/manifesto/clean_manifesto_parties.R"),
         finally = unlink("code/pledgeConfig"))
