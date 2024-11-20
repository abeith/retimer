

load('./data/mm1.rda')

expect_error(spectrogram(1:3))

expect_error(spectrogram("./file.mp4"))

expect_error(spectrogram(matrix(mm1@left, ncol = 1)))

seewave_spec <- spectrogram(mm1, method = "seewave")
phontools_spec <- spectrogram(mm1, method = "phonTools")
tuner_spec <- spectrogram(mm1, method = "tuneR")

par(mfrow = c(3,1))
with(seewave_spec, image(time, freq, amp, col = hcl.colors(20)))
with(phontools_spec, image(time, freq, amp, col = hcl.colors(20)))
with(tuner_spec, image(time, freq, amp, col = hcl.colors(20)))

