#ifndef PHILOX4X32_10_H
#define PHILOX4X32_10_H
#include <stdint.h>

#define PHILOX4X32_ROUNDS 10

// Philox constants - verified against reference implementation
static const uint32_t PHILOX_M4x32_0 = 0xD2511F53;
static const uint32_t PHILOX_M4x32_1 = 0xCD9E8D57;
static const uint32_t PHILOX_W32_0 = 0x9E3779B9;
static const uint32_t PHILOX_W32_1 = 0xBB67AE85;

static inline void
philox4x32_round(uint32_t ctr[4], uint32_t key[2])
{
    // Multiply and split
    uint64_t prod0 = (uint64_t)PHILOX_M4x32_0 * (uint64_t)ctr[0];
    uint64_t prod1 = (uint64_t)PHILOX_M4x32_1 * (uint64_t)ctr[2];

    uint32_t hi0 = (uint32_t)(prod0 >> 32);
    uint32_t hi1 = (uint32_t)(prod1 >> 32);
    uint32_t lo0 = (uint32_t)prod0;
    uint32_t lo1 = (uint32_t)prod1;

    // Try different permutation pattern - some implementations use this order
    uint32_t temp1 = ctr[1];
    uint32_t temp3 = ctr[3];

    ctr[0] = hi1 ^ temp1 ^ key[0];
    ctr[1] = lo1;
    ctr[2] = hi0 ^ temp3 ^ key[1];
    ctr[3] = lo0;
}

static inline void
philox4x32_10(uint32_t ctr[4], uint32_t key[2])
{
    // Make a copy of the key to avoid modifying the original
    uint32_t k[2] = { key[0], key[1] };

    for (int round = 0; round < PHILOX4X32_ROUNDS; ++round) {
        philox4x32_round(ctr, k);
        // Update key for next round
        k[0] += PHILOX_W32_0;
        k[1] += PHILOX_W32_1;
    }
}

// Alternative implementation matching Random123/NumPy reference
static inline void
philox4x32_10_alt(uint32_t ctr[4], uint32_t key[2])
{
    uint32_t k0 = key[0];
    uint32_t k1 = key[1];

    for (int round = 0; round < PHILOX4X32_ROUNDS; ++round) {
        // Philox uses a specific pattern: multiply ctr[0] and ctr[2]
        uint64_t prod0 = (uint64_t)PHILOX_M4x32_0 * (uint64_t)ctr[0];
        uint64_t prod1 = (uint64_t)PHILOX_M4x32_1 * (uint64_t)ctr[2];

        // Split products into high and low 32-bit parts
        uint32_t hi0 = (uint32_t)(prod0 >> 32);
        uint32_t lo0 = (uint32_t)(prod0);
        uint32_t hi1 = (uint32_t)(prod1 >> 32);
        uint32_t lo1 = (uint32_t)(prod1);

        // The critical permutation - based on Random123 reference
        // This is a Feistel-like network transformation
        uint32_t new_ctr0 = hi1 ^ ctr[1] ^ k0;
        uint32_t new_ctr1 = lo1;
        uint32_t new_ctr2 = hi0 ^ ctr[3] ^ k1;
        uint32_t new_ctr3 = lo0;

        ctr[0] = new_ctr0;
        ctr[1] = new_ctr1;
        ctr[2] = new_ctr2;
        ctr[3] = new_ctr3;

        // Bump keys for next round (Weyl sequence)
        k0 += PHILOX_W32_0;
        k1 += PHILOX_W32_1;
    }
}

#endif // PHILOX4X32_10_H
