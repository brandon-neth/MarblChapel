#ifndef JEMALLOC_INTERNAL_H
#define	JEMALLOC_INTERNAL_H

#include "jemalloc_internal_defs.h"
#include "jemalloc/internal/jemalloc_internal_decls.h"

#ifdef JEMALLOC_UTRACE
#include <sys/ktrace.h>
#endif

#define	JEMALLOC_NO_DEMANGLE
#ifdef JEMALLOC_JET
#  define JEMALLOC_N(n) jet_##n
#  include "jemalloc/internal/public_namespace.h"
#  define JEMALLOC_NO_RENAME
#  include "../jemalloc.h"
#  undef JEMALLOC_NO_RENAME
#else
#  define JEMALLOC_N(n) chpl_je_##n
#  include "../jemalloc.h"
#endif
#include "jemalloc/internal/private_namespace.h"

static const bool config_debug =
#ifdef JEMALLOC_DEBUG
    true
#else
    false
#endif
    ;
static const bool have_dss =
#ifdef JEMALLOC_DSS
    true
#else
    false
#endif
    ;
static const bool config_fill =
#ifdef JEMALLOC_FILL
    true
#else
    false
#endif
    ;
static const bool config_lazy_lock =
#ifdef JEMALLOC_LAZY_LOCK
    true
#else
    false
#endif
    ;
static const char * const config_malloc_conf = JEMALLOC_CONFIG_MALLOC_CONF;
static const bool config_prof =
#ifdef JEMALLOC_PROF
    true
#else
    false
#endif
    ;
static const bool config_prof_libgcc =
#ifdef JEMALLOC_PROF_LIBGCC
    true
#else
    false
#endif
    ;
static const bool config_prof_libunwind =
#ifdef JEMALLOC_PROF_LIBUNWIND
    true
#else
    false
#endif
    ;
static const bool maps_coalesce =
#ifdef JEMALLOC_MAPS_COALESCE
    true
#else
    false
#endif
    ;
static const bool config_munmap =
#ifdef JEMALLOC_MUNMAP
    true
#else
    false
#endif
    ;
static const bool config_stats =
#ifdef JEMALLOC_STATS
    true
#else
    false
#endif
    ;
static const bool config_tcache =
#ifdef JEMALLOC_TCACHE
    true
#else
    false
#endif
    ;
static const bool config_thp =
#ifdef JEMALLOC_THP
    true
#else
    false
#endif
    ;
static const bool config_tls =
#ifdef JEMALLOC_TLS
    true
#else
    false
#endif
    ;
static const bool config_utrace =
#ifdef JEMALLOC_UTRACE
    true
#else
    false
#endif
    ;
static const bool config_valgrind =
#ifdef JEMALLOC_VALGRIND
    true
#else
    false
#endif
    ;
static const bool config_xmalloc =
#ifdef JEMALLOC_XMALLOC
    true
#else
    false
#endif
    ;
static const bool config_ivsalloc =
#ifdef JEMALLOC_IVSALLOC
    true
#else
    false
#endif
    ;
static const bool config_cache_oblivious =
#ifdef JEMALLOC_CACHE_OBLIVIOUS
    true
#else
    false
#endif
    ;

#ifdef JEMALLOC_C11ATOMICS
#include <stdatomic.h>
#endif

#ifdef JEMALLOC_ATOMIC9
#include <machine/atomic.h>
#endif

#if (defined(JEMALLOC_OSATOMIC) || defined(JEMALLOC_OSSPIN))
#include <libkern/OSAtomic.h>
#endif

#ifdef JEMALLOC_ZONE
#include <mach/mach_error.h>
#include <mach/mach_init.h>
#include <mach/vm_map.h>
#endif

#include "jemalloc/internal/ph.h"
#ifndef __PGI
#define	RB_COMPACT
#endif
#include "jemalloc/internal/rb.h"
#include "jemalloc/internal/qr.h"
#include "jemalloc/internal/ql.h"

/*
 * jemalloc can conceptually be broken into components (arena, tcache, etc.),
 * but there are circular dependencies that cannot be broken without
 * substantial performance degradation.  In order to reduce the effect on
 * visual code flow, read the header files in multiple passes, with one of the
 * following cpp variables defined during each pass:
 *
 *   JEMALLOC_H_TYPES   : Preprocessor-defined constants and psuedo-opaque data
 *                        types.
 *   JEMALLOC_H_STRUCTS : Data structures.
 *   JEMALLOC_H_EXTERNS : Extern data declarations and function prototypes.
 *   JEMALLOC_H_INLINES : Inline functions.
 */
/******************************************************************************/
#define	JEMALLOC_H_TYPES

#include "jemalloc/internal/jemalloc_internal_macros.h"

/* Page size index type. */
typedef unsigned pszind_t;

/* Size class index type. */
typedef unsigned szind_t;

/*
 * Flags bits:
 *
 * a: arena
 * t: tcache
 * 0: unused
 * z: zero
 * n: alignment
 *
 * aaaaaaaa aaaatttt tttttttt 0znnnnnn
 */
#define	MALLOCX_ARENA_MASK	((int)~0xfffff)
#define	MALLOCX_ARENA_MAX	0xffe
#define	MALLOCX_TCACHE_MASK	((int)~0xfff000ffU)
#define	MALLOCX_TCACHE_MAX	0xffd
#define	MALLOCX_LG_ALIGN_MASK	((int)0x3f)
/* Use MALLOCX_ALIGN_GET() if alignment may not be specified in flags. */
#define	MALLOCX_ALIGN_GET_SPECIFIED(flags)				\
    (ZU(1) << (flags & MALLOCX_LG_ALIGN_MASK))
#define	MALLOCX_ALIGN_GET(flags)					\
    (MALLOCX_ALIGN_GET_SPECIFIED(flags) & (SIZE_T_MAX-1))
#define	MALLOCX_ZERO_GET(flags)						\
    ((bool)(flags & MALLOCX_ZERO))

#define	MALLOCX_TCACHE_GET(flags)					\
    (((unsigned)((flags & MALLOCX_TCACHE_MASK) >> 8)) - 2)
#define	MALLOCX_ARENA_GET(flags)					\
    (((unsigned)(((unsigned)flags) >> 20)) - 1)

/* Smallest size class to support. */
#define	TINY_MIN		(1U << LG_TINY_MIN)

/*
 * Minimum allocation alignment is 2^LG_QUANTUM bytes (ignoring tiny size
 * classes).
 */
#ifndef LG_QUANTUM
#  if (defined(__i386__) || defined(_M_IX86))
#    define LG_QUANTUM		4
#  endif
#  ifdef __ia64__
#    define LG_QUANTUM		4
#  endif
#  ifdef __alpha__
#    define LG_QUANTUM		4
#  endif
#  if (defined(__sparc64__) || defined(__sparcv9) || defined(__sparc_v9__))
#    define LG_QUANTUM		4
#  endif
#  if (defined(__amd64__) || defined(__x86_64__) || defined(_M_X64))
#    define LG_QUANTUM		4
#  endif
#  ifdef __arm__
#    define LG_QUANTUM		3
#  endif
#  ifdef __aarch64__
#    define LG_QUANTUM		4
#  endif
#  ifdef __hppa__
#    define LG_QUANTUM		4
#  endif
#  ifdef __mips__
#    define LG_QUANTUM		3
#  endif
#  ifdef __or1k__
#    define LG_QUANTUM		3
#  endif
#  ifdef __powerpc__
#    define LG_QUANTUM		4
#  endif
#  ifdef __riscv__
#    define LG_QUANTUM		4
#  endif
#  ifdef __s390__
#    define LG_QUANTUM		4
#  endif
#  ifdef __SH4__
#    define LG_QUANTUM		4
#  endif
#  ifdef __tile__
#    define LG_QUANTUM		4
#  endif
#  ifdef __le32__
#    define LG_QUANTUM		4
#  endif
#  ifndef LG_QUANTUM
#    error "Unknown minimum alignment for architecture; specify via "
	 "--with-lg-quantum"
#  endif
#endif

#define	QUANTUM			((size_t)(1U << LG_QUANTUM))
#define	QUANTUM_MASK		(QUANTUM - 1)

/* Return the smallest quantum multiple that is >= a. */
#define	QUANTUM_CEILING(a)						\
	(((a) + QUANTUM_MASK) & ~QUANTUM_MASK)

#define	LONG			((size_t)(1U << LG_SIZEOF_LONG))
#define	LONG_MASK		(LONG - 1)

/* Return the smallest long multiple that is >= a. */
#define	LONG_CEILING(a)							\
	(((a) + LONG_MASK) & ~LONG_MASK)

#define	SIZEOF_PTR		(1U << LG_SIZEOF_PTR)
#define	PTR_MASK		(SIZEOF_PTR - 1)

/* Return the smallest (void *) multiple that is >= a. */
#define	PTR_CEILING(a)							\
	(((a) + PTR_MASK) & ~PTR_MASK)

/*
 * Maximum size of L1 cache line.  This is used to avoid cache line aliasing.
 * In addition, this controls the spacing of cacheline-spaced size classes.
 *
 * CACHELINE cannot be based on LG_CACHELINE because __declspec(align()) can
 * only handle raw constants.
 */
#define	LG_CACHELINE		6
#define	CACHELINE		64
#define	CACHELINE_MASK		(CACHELINE - 1)

/* Return the smallest cacheline multiple that is >= s. */
#define	CACHELINE_CEILING(s)						\
	(((s) + CACHELINE_MASK) & ~CACHELINE_MASK)

/* Page size.  LG_PAGE is determined by the configure script. */
#ifdef PAGE_MASK
#  undef PAGE_MASK
#endif
#define	PAGE		((size_t)(1U << LG_PAGE))
#define	PAGE_MASK	((size_t)(PAGE - 1))

/* Return the page base address for the page containing address a. */
#define	PAGE_ADDR2BASE(a)						\
	((void *)((uintptr_t)(a) & ~PAGE_MASK))

/* Return the smallest pagesize multiple that is >= s. */
#define	PAGE_CEILING(s)							\
	(((s) + PAGE_MASK) & ~PAGE_MASK)

/* Return the nearest aligned address at or below a. */
#define	ALIGNMENT_ADDR2BASE(a, alignment)				\
	((void *)((uintptr_t)(a) & ((~(alignment)) + 1)))

/* Return the offset between a and the nearest aligned address at or below a. */
#define	ALIGNMENT_ADDR2OFFSET(a, alignment)				\
	((size_t)((uintptr_t)(a) & (alignment - 1)))

/* Return the smallest alignment multiple that is >= s. */
#define	ALIGNMENT_CEILING(s, alignment)					\
	(((s) + (alignment - 1)) & ((~(alignment)) + 1))

/* Declare a variable-length array. */
#if __STDC_VERSION__ < 199901L
#  ifdef _MSC_VER
#    include <malloc.h>
#    define alloca _alloca
#  else
#    ifdef JEMALLOC_HAS_ALLOCA_H
#      include <alloca.h>
#    else
#      include <stdlib.h>
#    endif
#  endif
#  define VARIABLE_ARRAY(type, name, count) \
	type *name = alloca(sizeof(type) * (count))
#else
#  define VARIABLE_ARRAY(type, name, count) type name[(count)]
#endif

#include "jemalloc/internal/nstime.h"
#include "jemalloc/internal/valgrind.h"
#include "jemalloc/internal/util.h"
#include "jemalloc/internal/atomic.h"
#include "jemalloc/internal/spin.h"
#include "jemalloc/internal/prng.h"
#include "jemalloc/internal/ticker.h"
#include "jemalloc/internal/ckh.h"
#include "jemalloc/internal/size_classes.h"
#include "jemalloc/internal/smoothstep.h"
#include "jemalloc/internal/stats.h"
#include "jemalloc/internal/ctl.h"
#include "jemalloc/internal/witness.h"
#include "jemalloc/internal/mutex.h"
#include "jemalloc/internal/tsd.h"
#include "jemalloc/internal/mb.h"
#include "jemalloc/internal/extent.h"
#include "jemalloc/internal/arena.h"
#include "jemalloc/internal/bitmap.h"
#include "jemalloc/internal/base.h"
#include "jemalloc/internal/rtree.h"
#include "jemalloc/internal/pages.h"
#include "jemalloc/internal/chunk.h"
#include "jemalloc/internal/huge.h"
#include "jemalloc/internal/tcache.h"
#include "jemalloc/internal/hash.h"
#include "jemalloc/internal/quarantine.h"
#include "jemalloc/internal/prof.h"

#undef JEMALLOC_H_TYPES
/******************************************************************************/
#define	JEMALLOC_H_STRUCTS

#include "jemalloc/internal/nstime.h"
#include "jemalloc/internal/valgrind.h"
#include "jemalloc/internal/util.h"
#include "jemalloc/internal/atomic.h"
#include "jemalloc/internal/spin.h"
#include "jemalloc/internal/prng.h"
#include "jemalloc/internal/ticker.h"
#include "jemalloc/internal/ckh.h"
#include "jemalloc/internal/size_classes.h"
#include "jemalloc/internal/smoothstep.h"
#include "jemalloc/internal/stats.h"
#include "jemalloc/internal/ctl.h"
#include "jemalloc/internal/witness.h"
#include "jemalloc/internal/mutex.h"
#include "jemalloc/internal/mb.h"
#include "jemalloc/internal/bitmap.h"
#define	JEMALLOC_ARENA_STRUCTS_A
#include "jemalloc/internal/arena.h"
#undef JEMALLOC_ARENA_STRUCTS_A
#include "jemalloc/internal/extent.h"
#define	JEMALLOC_ARENA_STRUCTS_B
#include "jemalloc/internal/arena.h"
#undef JEMALLOC_ARENA_STRUCTS_B
#include "jemalloc/internal/base.h"
#include "jemalloc/internal/rtree.h"
#include "jemalloc/internal/pages.h"
#include "jemalloc/internal/chunk.h"
#include "jemalloc/internal/huge.h"
#include "jemalloc/internal/tcache.h"
#include "jemalloc/internal/hash.h"
#include "jemalloc/internal/quarantine.h"
#include "jemalloc/internal/prof.h"

#include "jemalloc/internal/tsd.h"

#undef JEMALLOC_H_STRUCTS
/******************************************************************************/
#define	JEMALLOC_H_EXTERNS

extern bool	opt_abort;
extern const char	*opt_junk;
extern bool	opt_junk_alloc;
extern bool	opt_junk_free;
extern size_t	opt_quarantine;
extern bool	opt_redzone;
extern bool	opt_utrace;
extern bool	opt_xmalloc;
extern bool	opt_zero;
extern unsigned	opt_narenas;

extern bool	in_valgrind;

/* Number of CPUs. */
extern unsigned	ncpus;

/* Number of arenas used for automatic multiplexing of threads and arenas. */
extern unsigned	narenas_auto;

/*
 * Arenas that are used to service external requests.  Not all elements of the
 * arenas array are necessarily used; arenas are created lazily as needed.
 */
extern arena_t	**arenas;

/*
 * pind2sz_tab encodes the same information as could be computed by
 * pind2sz_compute().
 */
extern size_t const	pind2sz_tab[NPSIZES];
/*
 * index2size_tab encodes the same information as could be computed (at
 * unacceptable cost in some code paths) by index2size_compute().
 */
extern size_t const	index2size_tab[NSIZES];
/*
 * size2index_tab is a compact lookup table that rounds request sizes up to
 * size classes.  In order to reduce cache footprint, the table is compressed,
 * and all accesses are via size2index().
 */
extern uint8_t const	size2index_tab[];

arena_t	*a0get(void);
void	*a0malloc(size_t size);
void	a0dalloc(void *ptr);
void	*bootstrap_malloc(size_t size);
void	*bootstrap_calloc(size_t num, size_t size);
void	bootstrap_free(void *ptr);
unsigned	narenas_total_get(void);
arena_t	*arena_init(tsdn_t *tsdn, unsigned ind);
arena_tdata_t	*arena_tdata_get_hard(tsd_t *tsd, unsigned ind);
arena_t	*arena_choose_hard(tsd_t *tsd, bool internal);
void	arena_migrate(tsd_t *tsd, unsigned oldind, unsigned newind);
void	thread_allocated_cleanup(tsd_t *tsd);
void	thread_deallocated_cleanup(tsd_t *tsd);
void	iarena_cleanup(tsd_t *tsd);
void	arena_cleanup(tsd_t *tsd);
void	arenas_tdata_cleanup(tsd_t *tsd);
void	narenas_tdata_cleanup(tsd_t *tsd);
void	arenas_tdata_bypass_cleanup(tsd_t *tsd);
void	jemalloc_prefork(void);
void	jemalloc_postfork_parent(void);
void	jemalloc_postfork_child(void);

#include "jemalloc/internal/nstime.h"
#include "jemalloc/internal/valgrind.h"
#include "jemalloc/internal/util.h"
#include "jemalloc/internal/atomic.h"
#include "jemalloc/internal/spin.h"
#include "jemalloc/internal/prng.h"
#include "jemalloc/internal/ticker.h"
#include "jemalloc/internal/ckh.h"
#include "jemalloc/internal/size_classes.h"
#include "jemalloc/internal/smoothstep.h"
#include "jemalloc/internal/stats.h"
#include "jemalloc/internal/ctl.h"
#include "jemalloc/internal/witness.h"
#include "jemalloc/internal/mutex.h"
#include "jemalloc/internal/mb.h"
#include "jemalloc/internal/bitmap.h"
#include "jemalloc/internal/extent.h"
#include "jemalloc/internal/arena.h"
#include "jemalloc/internal/base.h"
#include "jemalloc/internal/rtree.h"
#include "jemalloc/internal/pages.h"
#include "jemalloc/internal/chunk.h"
#include "jemalloc/internal/huge.h"
#include "jemalloc/internal/tcache.h"
#include "jemalloc/internal/hash.h"
#include "jemalloc/internal/quarantine.h"
#include "jemalloc/internal/prof.h"
#include "jemalloc/internal/tsd.h"

#undef JEMALLOC_H_EXTERNS
/******************************************************************************/
#define	JEMALLOC_H_INLINES

#include "jemalloc/internal/nstime.h"
#include "jemalloc/internal/valgrind.h"
#include "jemalloc/internal/util.h"
#include "jemalloc/internal/atomic.h"
#include "jemalloc/internal/spin.h"
#include "jemalloc/internal/prng.h"
#include "jemalloc/internal/ticker.h"
#include "jemalloc/internal/ckh.h"
#include "jemalloc/internal/size_classes.h"
#include "jemalloc/internal/smoothstep.h"
#include "jemalloc/internal/stats.h"
#include "jemalloc/internal/ctl.h"
#include "jemalloc/internal/tsd.h"
#include "jemalloc/internal/witness.h"
#include "jemalloc/internal/mutex.h"
#include "jemalloc/internal/mb.h"
#include "jemalloc/internal/extent.h"
#include "jemalloc/internal/base.h"
#include "jemalloc/internal/rtree.h"
#include "jemalloc/internal/pages.h"
#include "jemalloc/internal/chunk.h"
#include "jemalloc/internal/huge.h"

#ifndef JEMALLOC_ENABLE_INLINE
pszind_t	psz2ind(size_t psz);
size_t	pind2sz_compute(pszind_t pind);
size_t	pind2sz_lookup(pszind_t pind);
size_t	pind2sz(pszind_t pind);
size_t	psz2u(size_t psz);
szind_t	size2index_compute(size_t size);
szind_t	size2index_lookup(size_t size);
szind_t	size2index(size_t size);
size_t	index2size_compute(szind_t index);
size_t	index2size_lookup(szind_t index);
size_t	index2size(szind_t index);
size_t	s2u_compute(size_t size);
size_t	s2u_lookup(size_t size);
size_t	s2u(size_t size);
size_t	sa2u(size_t size, size_t alignment);
arena_t	*arena_choose_impl(tsd_t *tsd, arena_t *arena, bool internal);
arena_t	*arena_choose(tsd_t *tsd, arena_t *arena);
arena_t	*arena_ichoose(tsd_t *tsd, arena_t *arena);
arena_tdata_t	*arena_tdata_get(tsd_t *tsd, unsigned ind,
    bool refresh_if_missing);
arena_t	*arena_get(tsdn_t *tsdn, unsigned ind, bool init_if_missing);
ticker_t	*decay_ticker_get(tsd_t *tsd, unsigned ind);
#endif

#if (defined(JEMALLOC_ENABLE_INLINE) || defined(JEMALLOC_C_))
JEMALLOC_INLINE pszind_t
psz2ind(size_t psz)
{

	if (unlikely(psz > HUGE_MAXCLASS))
		return (NPSIZES);
	{
		pszind_t x = lg_floor((psz<<1)-1);
		pszind_t shift = (x < LG_SIZE_CLASS_GROUP + LG_PAGE) ? 0 : x -
		    (LG_SIZE_CLASS_GROUP + LG_PAGE);
		pszind_t grp = shift << LG_SIZE_CLASS_GROUP;

		pszind_t lg_delta = (x < LG_SIZE_CLASS_GROUP + LG_PAGE + 1) ?
		    LG_PAGE : x - LG_SIZE_CLASS_GROUP - 1;

		size_t delta_inverse_mask = ZI(-1) << lg_delta;
		pszind_t mod = ((((psz-1) & delta_inverse_mask) >> lg_delta)) &
		    ((ZU(1) << LG_SIZE_CLASS_GROUP) - 1);

		pszind_t ind = grp + mod;
		return (ind);
	}
}

JEMALLOC_INLINE size_t
pind2sz_compute(pszind_t pind)
{

	{
		size_t grp = pind >> LG_SIZE_CLASS_GROUP;
		size_t mod = pind & ((ZU(1) << LG_SIZE_CLASS_GROUP) - 1);

		size_t grp_size_mask = ~((!!grp)-1);
		size_t grp_size = ((ZU(1) << (LG_PAGE +
		    (LG_SIZE_CLASS_GROUP-1))) << grp) & grp_size_mask;

		size_t shift = (grp == 0) ? 1 : grp;
		size_t lg_delta = shift + (LG_PAGE-1);
		size_t mod_size = (mod+1) << lg_delta;

		size_t sz = grp_size + mod_size;
		return (sz);
	}
}

JEMALLOC_INLINE size_t
pind2sz_lookup(pszind_t pind)
{
	size_t ret = (size_t)pind2sz_tab[pind];
	assert(ret == pind2sz_compute(pind));
	return (ret);
}

JEMALLOC_INLINE size_t
pind2sz(pszind_t pind)
{

	assert(pind < NPSIZES);
	return (pind2sz_lookup(pind));
}

JEMALLOC_INLINE size_t
psz2u(size_t psz)
{

	if (unlikely(psz > HUGE_MAXCLASS))
		return (0);
	{
		size_t x = lg_floor((psz<<1)-1);
		size_t lg_delta = (x < LG_SIZE_CLASS_GROUP + LG_PAGE + 1) ?
		    LG_PAGE : x - LG_SIZE_CLASS_GROUP - 1;
		size_t delta = ZU(1) << lg_delta;
		size_t delta_mask = delta - 1;
		size_t usize = (psz + delta_mask) & ~delta_mask;
		return (usize);
	}
}

JEMALLOC_INLINE szind_t
size2index_compute(size_t size)
{

	if (unlikely(size > HUGE_MAXCLASS))
		return (NSIZES);
#if (NTBINS != 0)
	if (size <= (ZU(1) << LG_TINY_MAXCLASS)) {
		szind_t lg_tmin = LG_TINY_MAXCLASS - NTBINS + 1;
		szind_t lg_ceil = lg_floor(pow2_ceil_zu(size));
		return (lg_ceil < lg_tmin ? 0 : lg_ceil - lg_tmin);
	}
#endif
	{
		szind_t x = lg_floor((size<<1)-1);
		szind_t shift = (x < LG_SIZE_CLASS_GROUP + LG_QUANTUM) ? 0 :
		    x - (LG_SIZE_CLASS_GROUP + LG_QUANTUM);
		szind_t grp = shift << LG_SIZE_CLASS_GROUP;

		szind_t lg_delta = (x < LG_SIZE_CLASS_GROUP + LG_QUANTUM + 1)
		    ? LG_QUANTUM : x - LG_SIZE_CLASS_GROUP - 1;

		size_t delta_inverse_mask = ZI(-1) << lg_delta;
		szind_t mod = ((((size-1) & delta_inverse_mask) >> lg_delta)) &
		    ((ZU(1) << LG_SIZE_CLASS_GROUP) - 1);

		szind_t index = NTBINS + grp + mod;
		return (index);
	}
}

JEMALLOC_ALWAYS_INLINE szind_t
size2index_lookup(size_t size)
{

	assert(size <= LOOKUP_MAXCLASS);
	{
		szind_t ret = (size2index_tab[(size-1) >> LG_TINY_MIN]);
		assert(ret == size2index_compute(size));
		return (ret);
	}
}

JEMALLOC_ALWAYS_INLINE szind_t
size2index(size_t size)
{

	assert(size > 0);
	if (likely(size <= LOOKUP_MAXCLASS))
		return (size2index_lookup(size));
	return (size2index_compute(size));
}

JEMALLOC_INLINE size_t
index2size_compute(szind_t index)
{

#if (NTBINS > 0)
	if (index < NTBINS)
		return (ZU(1) << (LG_TINY_MAXCLASS - NTBINS + 1 + index));
#endif
	{
		size_t reduced_index = index - NTBINS;
		size_t grp = reduced_index >> LG_SIZE_CLASS_GROUP;
		size_t mod = reduced_index & ((ZU(1) << LG_SIZE_CLASS_GROUP) -
		    1);

		size_t grp_size_mask = ~((!!grp)-1);
		size_t grp_size = ((ZU(1) << (LG_QUANTUM +
		    (LG_SIZE_CLASS_GROUP-1))) << grp) & grp_size_mask;

		size_t shift = (grp == 0) ? 1 : grp;
		size_t lg_delta = shift + (LG_QUANTUM-1);
		size_t mod_size = (mod+1) << lg_delta;

		size_t usize = grp_size + mod_size;
		return (usize);
	}
}

JEMALLOC_ALWAYS_INLINE size_t
index2size_lookup(szind_t index)
{
	size_t ret = (size_t)index2size_tab[index];
	assert(ret == index2size_compute(index));
	return (ret);
}

JEMALLOC_ALWAYS_INLINE size_t
index2size(szind_t index)
{

	assert(index < NSIZES);
	return (index2size_lookup(index));
}

JEMALLOC_ALWAYS_INLINE size_t
s2u_compute(size_t size)
{

	if (unlikely(size > HUGE_MAXCLASS))
		return (0);
#if (NTBINS > 0)
	if (size <= (ZU(1) << LG_TINY_MAXCLASS)) {
		size_t lg_tmin = LG_TINY_MAXCLASS - NTBINS + 1;
		size_t lg_ceil = lg_floor(pow2_ceil_zu(size));
		return (lg_ceil < lg_tmin ? (ZU(1) << lg_tmin) :
		    (ZU(1) << lg_ceil));
	}
#endif
	{
		size_t x = lg_floor((size<<1)-1);
		size_t lg_delta = (x < LG_SIZE_CLASS_GROUP + LG_QUANTUM + 1)
		    ?  LG_QUANTUM : x - LG_SIZE_CLASS_GROUP - 1;
		size_t delta = ZU(1) << lg_delta;
		size_t delta_mask = delta - 1;
		size_t usize = (size + delta_mask) & ~delta_mask;
		return (usize);
	}
}

JEMALLOC_ALWAYS_INLINE size_t
s2u_lookup(size_t size)
{
	size_t ret = index2size_lookup(size2index_lookup(size));

	assert(ret == s2u_compute(size));
	return (ret);
}

/*
 * Compute usable size that would result from allocating an object with the
 * specified size.
 */
JEMALLOC_ALWAYS_INLINE size_t
s2u(size_t size)
{

	assert(size > 0);
	if (likely(size <= LOOKUP_MAXCLASS))
		return (s2u_lookup(size));
	return (s2u_compute(size));
}

/*
 * Compute usable size that would result from allocating an object with the
 * specified size and alignment.
 */
JEMALLOC_ALWAYS_INLINE size_t
sa2u(size_t size, size_t alignment)
{
	size_t usize;

	assert(alignment != 0 && ((alignment - 1) & alignment) == 0);

	/* Try for a small size class. */
	if (size <= SMALL_MAXCLASS && alignment < PAGE) {
		/*
		 * Round size up to the nearest multiple of alignment.
		 *
		 * This done, we can take advantage of the fact that for each
		 * small size class, every object is aligned at the smallest
		 * power of two that is non-zero in the base two representation
		 * of the size.  For example:
		 *
		 *   Size |   Base 2 | Minimum alignment
		 *   -----+----------+------------------
		 *     96 |  1100000 |  32
		 *    144 | 10100000 |  32
		 *    192 | 11000000 |  64
		 */
		usize = s2u(ALIGNMENT_CEILING(size, alignment));
		if (usize < LARGE_MINCLASS)
			return (usize);
	}

	/* Try for a large size class. */
	if (likely(size <= large_maxclass) && likely(alignment < chunksize)) {
		/*
		 * We can't achieve subpage alignment, so round up alignment
		 * to the minimum that can actually be supported.
		 */
		alignment = PAGE_CEILING(alignment);

		/* Make sure result is a large size class. */
		usize = (size <= LARGE_MINCLASS) ? LARGE_MINCLASS : s2u(size);

		/*
		 * Calculate the size of the over-size run that arena_palloc()
		 * would need to allocate in order to guarantee the alignment.
		 */
		if (usize + large_pad + alignment - PAGE <= arena_maxrun)
			return (usize);
	}

	/* Huge size class.  Beware of overflow. */

	if (unlikely(alignment > HUGE_MAXCLASS))
		return (0);

	/*
	 * We can't achieve subchunk alignment, so round up alignment to the
	 * minimum that can actually be supported.
	 */
	alignment = CHUNK_CEILING(alignment);

	/* Make sure result is a huge size class. */
	if (size <= chunksize)
		usize = chunksize;
	else {
		usize = s2u(size);
		if (usize < size) {
			/* size_t overflow. */
			return (0);
		}
	}

	/*
	 * Calculate the multi-chunk mapping that huge_palloc() would need in
	 * order to guarantee the alignment.
	 */
	if (usize + alignment - PAGE < usize) {
		/* size_t overflow. */
		return (0);
	}
	return (usize);
}

/* Choose an arena based on a per-thread value. */
JEMALLOC_INLINE arena_t *
arena_choose_impl(tsd_t *tsd, arena_t *arena, bool internal)
{
	arena_t *ret;

	if (arena != NULL)
		return (arena);

	ret = internal ? tsd_iarena_get(tsd) : tsd_arena_get(tsd);
	if (unlikely(ret == NULL))
		ret = arena_choose_hard(tsd, internal);

	return (ret);
}

JEMALLOC_INLINE arena_t *
arena_choose(tsd_t *tsd, arena_t *arena)
{

	return (arena_choose_impl(tsd, arena, false));
}

JEMALLOC_INLINE arena_t *
arena_ichoose(tsd_t *tsd, arena_t *arena)
{

	return (arena_choose_impl(tsd, arena, true));
}

JEMALLOC_INLINE arena_tdata_t *
arena_tdata_get(tsd_t *tsd, unsigned ind, bool refresh_if_missing)
{
	arena_tdata_t *tdata;
	arena_tdata_t *arenas_tdata = tsd_arenas_tdata_get(tsd);

	if (unlikely(arenas_tdata == NULL)) {
		/* arenas_tdata hasn't been initialized yet. */
		return (arena_tdata_get_hard(tsd, ind));
	}
	if (unlikely(ind >= tsd_narenas_tdata_get(tsd))) {
		/*
		 * ind is invalid, cache is old (too small), or tdata to be
		 * initialized.
		 */
		return (refresh_if_missing ? arena_tdata_get_hard(tsd, ind) :
		    NULL);
	}

	tdata = &arenas_tdata[ind];
	if (likely(tdata != NULL) || !refresh_if_missing)
		return (tdata);
	return (arena_tdata_get_hard(tsd, ind));
}

JEMALLOC_INLINE arena_t *
arena_get(tsdn_t *tsdn, unsigned ind, bool init_if_missing)
{
	arena_t *ret;

	assert(ind <= MALLOCX_ARENA_MAX);

	ret = arenas[ind];
	if (unlikely(ret == NULL)) {
		ret = atomic_read_p((void *)&arenas[ind]);
		if (init_if_missing && unlikely(ret == NULL))
			ret = arena_init(tsdn, ind);
	}
	return (ret);
}

JEMALLOC_INLINE ticker_t *
decay_ticker_get(tsd_t *tsd, unsigned ind)
{
	arena_tdata_t *tdata;

	tdata = arena_tdata_get(tsd, ind, true);
	if (unlikely(tdata == NULL))
		return (NULL);
	return (&tdata->decay_ticker);
}
#endif

#include "jemalloc/internal/bitmap.h"
/*
 * Include portions of arena.h interleaved with tcache.h in order to resolve
 * circular dependencies.
 */
#define	JEMALLOC_ARENA_INLINE_A
#include "jemalloc/internal/arena.h"
#undef JEMALLOC_ARENA_INLINE_A
#include "jemalloc/internal/tcache.h"
#define	JEMALLOC_ARENA_INLINE_B
#include "jemalloc/internal/arena.h"
#undef JEMALLOC_ARENA_INLINE_B
#include "jemalloc/internal/hash.h"
#include "jemalloc/internal/quarantine.h"

#ifndef JEMALLOC_ENABLE_INLINE
arena_t	*iaalloc(const void *ptr);
size_t	isalloc(tsdn_t *tsdn, const void *ptr, bool demote);
void	*iallocztm(tsdn_t *tsdn, size_t size, szind_t ind, bool zero,
    tcache_t *tcache, bool is_metadata, arena_t *arena, bool slow_path);
void	*ialloc(tsd_t *tsd, size_t size, szind_t ind, bool zero,
    bool slow_path);
void	*ipallocztm(tsdn_t *tsdn, size_t usize, size_t alignment, bool zero,
    tcache_t *tcache, bool is_metadata, arena_t *arena);
void	*ipalloct(tsdn_t *tsdn, size_t usize, size_t alignment, bool zero,
    tcache_t *tcache, arena_t *arena);
void	*ipalloc(tsd_t *tsd, size_t usize, size_t alignment, bool zero);
size_t	ivsalloc(tsdn_t *tsdn, const void *ptr, bool demote);
size_t	u2rz(size_t usize);
size_t	p2rz(tsdn_t *tsdn, const void *ptr);
void	idalloctm(tsdn_t *tsdn, void *ptr, tcache_t *tcache, bool is_metadata,
    bool slow_path);
void	idalloc(tsd_t *tsd, void *ptr);
void	iqalloc(tsd_t *tsd, void *ptr, tcache_t *tcache, bool slow_path);
void	isdalloct(tsdn_t *tsdn, void *ptr, size_t size, tcache_t *tcache,
    bool slow_path);
void	isqalloc(tsd_t *tsd, void *ptr, size_t size, tcache_t *tcache,
    bool slow_path);
void	*iralloct_realign(tsd_t *tsd, void *ptr, size_t oldsize, size_t size,
    size_t extra, size_t alignment, bool zero, tcache_t *tcache,
    arena_t *arena);
void	*iralloct(tsd_t *tsd, void *ptr, size_t oldsize, size_t size,
    size_t alignment, bool zero, tcache_t *tcache, arena_t *arena);
void	*iralloc(tsd_t *tsd, void *ptr, size_t oldsize, size_t size,
    size_t alignment, bool zero);
bool	ixalloc(tsdn_t *tsdn, void *ptr, size_t oldsize, size_t size,
    size_t extra, size_t alignment, bool zero);
#endif

#if (defined(JEMALLOC_ENABLE_INLINE) || defined(JEMALLOC_C_))
JEMALLOC_ALWAYS_INLINE arena_t *
iaalloc(const void *ptr)
{

	assert(ptr != NULL);

	return (arena_aalloc(ptr));
}

/*
 * Typical usage:
 *   tsdn_t *tsdn = [...]
 *   void *ptr = [...]
 *   size_t sz = isalloc(tsdn, ptr, config_prof);
 */
JEMALLOC_ALWAYS_INLINE size_t
isalloc(tsdn_t *tsdn, const void *ptr, bool demote)
{

	assert(ptr != NULL);
	/* Demotion only makes sense if config_prof is true. */
	assert(config_prof || !demote);

	return (arena_salloc(tsdn, ptr, demote));
}

JEMALLOC_ALWAYS_INLINE void *
iallocztm(tsdn_t *tsdn, size_t size, szind_t ind, bool zero, tcache_t *tcache,
    bool is_metadata, arena_t *arena, bool slow_path)
{
	void *ret;

	assert(size != 0);
	assert(!is_metadata || tcache == NULL);
	assert(!is_metadata || arena == NULL || arena->ind < narenas_auto);

	ret = arena_malloc(tsdn, arena, size, ind, zero, tcache, slow_path);
	if (config_stats && is_metadata && likely(ret != NULL)) {
		arena_metadata_allocated_add(iaalloc(ret),
		    isalloc(tsdn, ret, config_prof));
	}
	return (ret);
}

JEMALLOC_ALWAYS_INLINE void *
ialloc(tsd_t *tsd, size_t size, szind_t ind, bool zero, bool slow_path)
{

	return (iallocztm(tsd_tsdn(tsd), size, ind, zero, tcache_get(tsd, true),
	    false, NULL, slow_path));
}

JEMALLOC_ALWAYS_INLINE void *
ipallocztm(tsdn_t *tsdn, size_t usize, size_t alignment, bool zero,
    tcache_t *tcache, bool is_metadata, arena_t *arena)
{
	void *ret;

	assert(usize != 0);
	assert(usize == sa2u(usize, alignment));
	assert(!is_metadata || tcache == NULL);
	assert(!is_metadata || arena == NULL || arena->ind < narenas_auto);

	ret = arena_palloc(tsdn, arena, usize, alignment, zero, tcache);
	assert(ALIGNMENT_ADDR2BASE(ret, alignment) == ret);
	if (config_stats && is_metadata && likely(ret != NULL)) {
		arena_metadata_allocated_add(iaalloc(ret), isalloc(tsdn, ret,
		    config_prof));
	}
	return (ret);
}

JEMALLOC_ALWAYS_INLINE void *
ipalloct(tsdn_t *tsdn, size_t usize, size_t alignment, bool zero,
    tcache_t *tcache, arena_t *arena)
{

	return (ipallocztm(tsdn, usize, alignment, zero, tcache, false, arena));
}

JEMALLOC_ALWAYS_INLINE void *
ipalloc(tsd_t *tsd, size_t usize, size_t alignment, bool zero)
{

	return (ipallocztm(tsd_tsdn(tsd), usize, alignment, zero,
	    tcache_get(tsd, true), false, NULL));
}

JEMALLOC_ALWAYS_INLINE size_t
ivsalloc(tsdn_t *tsdn, const void *ptr, bool demote)
{
	extent_node_t *node;

	/* Return 0 if ptr is not within a chunk managed by jemalloc. */
	node = chunk_lookup(ptr, false);
	if (node == NULL)
		return (0);
	/* Only arena chunks should be looked up via interior pointers. */
	assert(extent_node_addr_get(node) == ptr ||
	    extent_node_achunk_get(node));

	return (isalloc(tsdn, ptr, demote));
}

JEMALLOC_INLINE size_t
u2rz(size_t usize)
{
	size_t ret;

	if (usize <= SMALL_MAXCLASS) {
		szind_t binind = size2index(usize);
		ret = arena_bin_info[binind].redzone_size;
	} else
		ret = 0;

	return (ret);
}

JEMALLOC_INLINE size_t
p2rz(tsdn_t *tsdn, const void *ptr)
{
	size_t usize = isalloc(tsdn, ptr, false);

	return (u2rz(usize));
}

JEMALLOC_ALWAYS_INLINE void
idalloctm(tsdn_t *tsdn, void *ptr, tcache_t *tcache, bool is_metadata,
    bool slow_path)
{

	assert(ptr != NULL);
	assert(!is_metadata || tcache == NULL);
	assert(!is_metadata || iaalloc(ptr)->ind < narenas_auto);
	if (config_stats && is_metadata) {
		arena_metadata_allocated_sub(iaalloc(ptr), isalloc(tsdn, ptr,
		    config_prof));
	}

	arena_dalloc(tsdn, ptr, tcache, slow_path);
}

JEMALLOC_ALWAYS_INLINE void
idalloc(tsd_t *tsd, void *ptr)
{

	idalloctm(tsd_tsdn(tsd), ptr, tcache_get(tsd, false), false, true);
}

JEMALLOC_ALWAYS_INLINE void
iqalloc(tsd_t *tsd, void *ptr, tcache_t *tcache, bool slow_path)
{

	if (slow_path && config_fill && unlikely(opt_quarantine))
		quarantine(tsd, ptr);
	else
		idalloctm(tsd_tsdn(tsd), ptr, tcache, false, slow_path);
}

JEMALLOC_ALWAYS_INLINE void
isdalloct(tsdn_t *tsdn, void *ptr, size_t size, tcache_t *tcache,
    bool slow_path)
{

	arena_sdalloc(tsdn, ptr, size, tcache, slow_path);
}

JEMALLOC_ALWAYS_INLINE void
isqalloc(tsd_t *tsd, void *ptr, size_t size, tcache_t *tcache, bool slow_path)
{

	if (slow_path && config_fill && unlikely(opt_quarantine))
		quarantine(tsd, ptr);
	else
		isdalloct(tsd_tsdn(tsd), ptr, size, tcache, slow_path);
}

JEMALLOC_ALWAYS_INLINE void *
iralloct_realign(tsd_t *tsd, void *ptr, size_t oldsize, size_t size,
    size_t extra, size_t alignment, bool zero, tcache_t *tcache, arena_t *arena)
{
	void *p;
	size_t usize, copysize;

	usize = sa2u(size + extra, alignment);
	if (unlikely(usize == 0 || usize > HUGE_MAXCLASS))
		return (NULL);
	p = ipalloct(tsd_tsdn(tsd), usize, alignment, zero, tcache, arena);
	if (p == NULL) {
		if (extra == 0)
			return (NULL);
		/* Try again, without extra this time. */
		usize = sa2u(size, alignment);
		if (unlikely(usize == 0 || usize > HUGE_MAXCLASS))
			return (NULL);
		p = ipalloct(tsd_tsdn(tsd), usize, alignment, zero, tcache,
		    arena);
		if (p == NULL)
			return (NULL);
	}
	/*
	 * Copy at most size bytes (not size+extra), since the caller has no
	 * expectation that the extra bytes will be reliably preserved.
	 */
	copysize = (size < oldsize) ? size : oldsize;
	memcpy(p, ptr, copysize);
	isqalloc(tsd, ptr, oldsize, tcache, true);
	return (p);
}

JEMALLOC_ALWAYS_INLINE void *
iralloct(tsd_t *tsd, void *ptr, size_t oldsize, size_t size, size_t alignment,
    bool zero, tcache_t *tcache, arena_t *arena)
{

	assert(ptr != NULL);
	assert(size != 0);

	if (alignment != 0 && ((uintptr_t)ptr & ((uintptr_t)alignment-1))
	    != 0) {
		/*
		 * Existing object alignment is inadequate; allocate new space
		 * and copy.
		 */
		return (iralloct_realign(tsd, ptr, oldsize, size, 0, alignment,
		    zero, tcache, arena));
	}

	return (arena_ralloc(tsd, arena, ptr, oldsize, size, alignment, zero,
	    tcache));
}

JEMALLOC_ALWAYS_INLINE void *
iralloc(tsd_t *tsd, void *ptr, size_t oldsize, size_t size, size_t alignment,
    bool zero)
{

	return (iralloct(tsd, ptr, oldsize, size, alignment, zero,
	    tcache_get(tsd, true), NULL));
}

JEMALLOC_ALWAYS_INLINE bool
ixalloc(tsdn_t *tsdn, void *ptr, size_t oldsize, size_t size, size_t extra,
    size_t alignment, bool zero)
{

	assert(ptr != NULL);
	assert(size != 0);

	if (alignment != 0 && ((uintptr_t)ptr & ((uintptr_t)alignment-1))
	    != 0) {
		/* Existing object alignment is inadequate. */
		return (true);
	}

	return (arena_ralloc_no_move(tsdn, ptr, oldsize, size, extra, zero));
}
#endif

#include "jemalloc/internal/prof.h"

#undef JEMALLOC_H_INLINES
/******************************************************************************/
#endif /* JEMALLOC_INTERNAL_H */
