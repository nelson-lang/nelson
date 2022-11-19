#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use std::os::raw::c_int;

// required mex types
pub struct mxArray_tag;
type mxArray = mxArray_tag;

#[link(name="libnlsMex")]
extern "C" {
    fn mexPrintf(fmt: *const u8, ...);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn mexFunction(nlhs: c_int,
                              plhs: *mut *mut mxArray,
                              nrhs: c_int,
                              prhs: *mut *mut mxArray) {
    unsafe {
        mexPrintf(b"Hello World in a mex from Nelson.\n\0".as_ptr());
    }
}
