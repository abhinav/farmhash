#ifndef FARMHASH_C_H_
#define FARMHASH_C_H_

#include <stdlib.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

size_t Hash(const char* s, size_t len);
uint32_t Hash32(const char* s, size_t len);
uint32_t Hash32WithSeed(const char* s, size_t len, uint32_t seed);

uint64_t Hash64(const char* s, size_t len);
uint64_t Hash64WithSeed(const char* s, size_t len, uint64_t seed);
uint64_t Hash64WithSeeds(const char* s, size_t len,
                       uint64_t seed0, uint64_t seed1);

uint32_t Fingerprint32(const char* s, size_t len);
uint64_t Fingerprint64(const char* s, size_t len);

#ifdef __cplusplus
}
#endif

#endif /* end of include guard: FARMHASH_C_H_ */
