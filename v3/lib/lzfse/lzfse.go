package lzfse

import "modernc.org/libc"

func fse_extract_bits64(tls *libc.TLS, x uint64_t, start fse_bit_count, nbits fse_bit_count) uint64_t {
	return fse_mask_lsb64(tls, x>>start, nbits)
}
