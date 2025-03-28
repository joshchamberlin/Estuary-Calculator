
# Function writes a BibTex file which can then be thrown into
# a citation manager to quickly cite packages used.
write_bib(
  x = .packages(),
  file = "code/creating_metadata/packages.bib",
  tweak = T,
  width = 60
)