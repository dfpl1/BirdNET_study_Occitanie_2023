for f in */*.sh; do
  bash "$f" 
done

for f in */*/*/*.sh; do
  bash "$f" 
done
