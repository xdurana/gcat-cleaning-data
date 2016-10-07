namespace :datacleaning do

  SCRIPTS_DIR = "/home/labs/dnalab/share/lims/R/gcat-cleaning-data"
  
  task :clean do
    Dir.chdir(SCRIPTS_DIR) do
      sh %{source bin/jupyter.sh}
    end
  end

end