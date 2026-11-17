---
name: nelson-usage-signal-fft
audience: user
modules: signal_processing, fftw
description: Analyze and filter signals in Nelson - FFT spectra, convolution, digital filters, windows and correlation. Use when the user computes a spectrum, filters a signal, or correlates two signals.
triggers: signal, fft, spectre, spectrum, frequence, frequency, transformee, transform, fourier, filtre, filter, convolution, conv, correlation, xcorr, fenetre, window, hamming, hann, butter, freqz, periodogram, spectrogram, reechantillonner, resample
functions: fft, ifft, fft2, ifft2, fftshift, ifftshift, conv, conv2, filter, filter2, abs, angle, real, imag, xcorr, xcov, hamming, hann, blackman, butter, fir1, freqz, periodogram, spectrogram, resample, downsample
---

# Signal Processing and FFT

Get a spectrum, filter a signal, or correlate two signals, with the frequency axis built correctly.

## When to use
- Computing a spectrum / frequency content (`fft`).
- Filtering (convolution or a designed digital filter).
- Correlating signals, windowing, resampling.

## When NOT to use
- Plotting the result (use the plotting guide once computed).

## Key functions
Transform: `fft`, `ifft`, `fft2`, `ifft2`, `fftshift`, `ifftshift`. Magnitude/phase: `abs`, `angle`, `real`, `imag`. Filter: `conv`, `conv2`, `filter`, with designs `butter`, `fir1` and response `freqz`. Windows: `hamming`, `hann`, `blackman`. Estimate/relate: `periodogram`, `spectrogram`, `xcorr`, `xcov`. Rate: `resample`, `downsample`. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. Build the frequency axis from the sample rate and length: a single-sided spectrum is `abs(fft(x))` indexed by `f = (0:N-1)*(fs/N)`; use `fftshift` for a centered two-sided view.
2. Window before an FFT of a finite segment (`hamming`/`hann`) to reduce leakage.
3. Apply a designed filter with `filter(b, a, x)`; check its shape with `freqz` first.
4. Correlate with `xcorr`; note the lag axis is centered on zero.
5. Keep magnitude and phase separate (`abs` and `angle`); do not plot the raw complex result.

## Gotchas
- `fft` returns a complex spectrum; take `abs`/`angle`, and scale by `1/N` for amplitudes.
- `conv` grows the length (`length(x)+length(h)-1`); use `'same'` when you need the input length.
- Resampling changes the time base; recompute the time/frequency axis afterwards.

## Done when
The spectrum, filtered signal, or correlation is computed with a correctly scaled frequency/lag axis and magnitude/phase handled separately.
