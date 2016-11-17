#!bin/sh

source /soft/general/virtenvs/jupyter/bin/activate

ipython nbconvert --execute --to html notebooks/Filter\ age\ and\ birth\ date.ipynb
ipython nbconvert --execute --to html notebooks/Filter\ gender.ipynb
ipython nbconvert --execute --to html notebooks/Filter\ height\ and\ weight.ipynb
ipython nbconvert --execute --to html notebooks/Filter\ hip\ and\ waist.ipynb
ipython nbconvert --execute --to html notebooks/Filter\ blood\ pressure.ipynb
ipython nbconvert --execute --to html notebooks/Filter\ locations.ipynb
ipython nbconvert --execute --to html notebooks/Filter\ participants.ipynb

python src/Filter\ medication.py

ipython nbconvert --execute --to html notebooks/Merge\ clean\ datasets.ipynb

deactivate
