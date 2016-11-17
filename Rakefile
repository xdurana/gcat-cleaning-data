namespace :datacleaning do

  SCRIPTS_DIR = "/home/labs/dnalab/share/lims/R/gcat-cleaning-data"
  
  task :conditions do
    Dir.chdir(SCRIPTS_DIR) do
      sh %{/soft/bin/Rscript R/conditions.R run}
    end
  end

  task :clean do
    Dir.chdir(SCRIPTS_DIR) do
      system %{source exec/jupyter.sh}
    end
  end

end