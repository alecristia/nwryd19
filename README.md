# nwryd19

This repository contains historical and in-progress versions of the manuscript "Non-word repetition in Yélî Dnye". For archival versions, please visit https://osf.io/5qspb/

## Explanation of contents

### Original files

These are 'raw' files, they cannot be created from other materials

- NWR-transcription.txt: actual transcription
- final_order.txt: correspondence between an nwr and its order of presentation for transcription
- pl-With_yd.tsv: previous literature results including also results for the present study
- rossel-ortho-replacements.txt: correspondence between ortho representation and unicode representation used for some analyses
- words_corpus.txt: words extracted from a YD corpus
- getting_frequency_corpus.Rmd: routine to extract phoneme token and type frequency from words_corpus.txt
- lastknit_session_info.txt: encodes information about environment in which manuscript.rmd was last knit (for reproducibility)
- segments.xlsx: ortho, phono, unicode representation of phonemes as well as their cross-linguistic frequency from PHOIBLE
- stimuli.xlsx: description of the stimuli
- nwr-demo.csv: demographic information of participants
- manuscript.Rmd: file to regenerate the manuscript
- wrangling.R: file that combines datasets for analyses in the manuscripts
- final_data_fields.txt: field explanations for final_data.txt

### Derived files


#### Created by wrangling.R

- match_bank_try1.txt: each line represents a phone that was pronounced correctly, taking into account only the first attempt; column names as in final_data_fields.txt 
- match_bank.txt: each line represents a phone that was pronounced correctly, taking into account all attempts; column names as in final_data_fields.txt
- deletion_bank_try1.txt: each line represents a phone that was deleted, taking into account only the first attempt; column names as in final_data_fields.txt
- deletion_bank.txt: each line represents a phone that was deleted, taking into account all attempts; column names as in final_data_fields.txt
- substitution_bank_try1.txt: each line represents a phone that was changed, together with what it was changed into, taking into account only the first attempt; column names as in final_data_fields.txt
- substitution_bank.txt: each line represents a phone that was changed, together with what it was changed into, taking into account all attempts; column names as in final_data_fields.txt
- final_data.txt: final data set combining all information together; see final_data_fields.txt for field explanations
- segments_with_cor_freq.txt: intermediate file which combines phonemes' characteristics with cross-linguistic and corpus frequencies

#### Created by getting_frequency_corpus.Rmd

- segment-counts-types.txt: type counts for phonemes in a YD corpus
- segment-counts.txt: token counts for phonemes in a YD corpus

