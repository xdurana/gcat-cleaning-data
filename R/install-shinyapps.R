install.packages('PKI')
devtools::install_github("s-u/pki")

install.packages('rsconnect')

rsconnect::setAccountInfo(name='xdurana',
                          token='69E06A548DA89090CEA00DA89414EAD6',
                          secret='QQkph62dZlSEJ2R/ENjENFg9codc548MReZoddwJ')

rsconnect::deployApp('R/rules')