#ifndef TAG_H
#define TAG_H

#ifdef D_STACK_SIZE
#define default_stacksize D_STACK_SIZE
#else
#define default_stacksize 10000000 /* 10000000B = 10MB */
#endif

#ifdef D_HEAP_SIZE
#define default_heapsize D_HEAP_SIZE
#else
#define default_heapsize 10000000
#endif

#define word_size              8
#define size_fixnum            1
#define object_alignment       8
#define shift_fixnum           3
#define mask_fixnum            7
#define tag_fixnum             0
#define mask_pair              7
#define tag_pair               1
#define size_pair              2
#define disp_car               0
#define disp_cdr               8
#define mask_vector            7
#define tag_vector             3
#define disp_vector_length     0
#define disp_vector_data       8
#define mask_procedure         7
#define tag_procedure          2
#define disp_procedure_code    0
#define disp_procedure_length  8
#define disp_procedure_data   16
#define mask_box               7
#define tag_box                4
#define size_box               8
#define disp_box_data          0
#define mask_string            7
#define tag_string             5
#define disp_string_data       0
#define mask_boolean           7
#define tag_boolean            6
#define _false                 6
#define _true                 14
#define _nil                  22
#define _void                 30
#define basic_ptr_tag_mask    7L

typedef long ptr;

#define VALUE(x)        (x & (~((long) mask_fixnum)))

#define DEREF(x)        (* ((ptr*) VALUE(x)))
#define UNFIX(x)        (x >> shift_fixnum)
#define FIX(x)          (x << shift_fixnum)
#define TAG(x,mask)     (x & mask)
#define UNTAG(x,tag)    (((long)x) - tag)
#define CAR(x)          (*(ptr *)(UNTAG(x,tag_pair) + disp_car))
#define CDR(x)          (*(ptr *)(UNTAG(x,tag_pair) + disp_cdr))
#define VECTORLENGTH(x) (* (ptr *)(UNTAG(x,tag_vector) + disp_vector_length))
#define VECTORDATA(x)   ((ptr *)(UNTAG(x, tag_vector) + disp_vector_data))
#define VECSIZE(x)      ((1 + UNFIX(VECTORLENGTH(x))))
#define PROCCODE(x)     (*((ptr *)(UNTAG(x,tag_procedure)) + disp_procedure_code))
#define PROCLENGTH(x)   (*((ptr *)(UNTAG(x,tag_procedure)) + disp_procedure_length))
#define PROCDATA(x)     ((ptr *) (UNTAG(x,tag_procedure) + disp_procedure_data))
#define PROCSIZE(x)     ((UNFIX(PROCLENGTH(x)))+2 )
#define BOXDATA(x)      ((ptr *)(UNTAG(x, tag_box) + disp_box_data))
#define STRINGDATA(x)   ((char *)(UNTAG(x, tag_string) >> shift_fixnum));

typedef union _rt_obj* rt_ptr;

typedef struct{
  rt_ptr data;
} rt_box;

typedef struct{
  rt_ptr car;
  rt_ptr cdr;
} rt_pair;

typedef struct{
  void  *code;
  ptr   length;
  ptr   data[];
} rt_procedure;

typedef struct{
  ptr length;
  ptr data[];
} rt_vector;

typedef union _rt_obj{
  ptr            fwd_ptr;
  ptr            *fwd_p;
  rt_pair        pair;
  rt_procedure   procedure;
  rt_vector      vector;
  rt_box         box;
} rt_obj;

/* untag a ptr and cast to rt_ptr union */
#define AS_RT_PTR(x) ((rt_ptr) (x & ~basic_ptr_tag_mask))
/* gcc.gnu.org/onlinedocs/gcc/Cast-to-Union.html */
#endif
