namespace :datacleaning do

  SCRIPTS_DIR = "/home/labs/dnalab/share/lims/R/gcat-cleaning-data"
  
  task :check do
    Dir.chdir(SCRIPTS_DIR) do
      sh %{/soft/bin/Rscript R/check/check.R}
    end
  end

  task :dataset_gcat do
    Dir.chdir(SCRIPTS_DIR) do
      sh %{/soft/bin/Rscript R/datasets/gcat/data.R}
    end
  end

  task :dataset_gcat_imputed do
    Dir.chdir(SCRIPTS_DIR) do
      sh %{/soft/bin/Rscript R/datasets/gcat-imputed/data.R}
    end
  end

  task :dataset_gcat_core do
    Dir.chdir(SCRIPTS_DIR) do
      sh %{/soft/bin/Rscript R/datasets/gcat-core/data.R}
    end
  end

  task :summary_report do
    Dir.chdir(SCRIPTS_DIR) do
      sh %{/soft/bin/Rscript R/summary-report.R}
    end
  end

end