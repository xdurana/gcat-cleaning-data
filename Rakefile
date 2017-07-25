namespace :datacleaning do

  SCRIPTS_DIR = "/home/labs/dnalab/share/lims/R/gcat-cleaning-data"
  
  task :check do
    Dir.chdir(SCRIPTS_DIR) do
      sh %{/soft/bin/Rscript R/check.R run}
      sh %{/soft/bin/Rscript R/summary-report.R}
    end
  end

end