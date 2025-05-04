use std::alloc::{alloc, dealloc, Layout};
use std::ptr;

pub struct Arena {
    buffer: *mut u8,
    capacity: usize,
    used: usize,
    layout: Layout,
}

#[no_mangle]
pub extern "C" fn arena_create(capacity: usize) -> *mut Arena {
    let layout = match Layout::from_size_align(capacity, 8) {
        Ok(l) => l,
        Err(_) => return ptr::null_mut(),
    };

    unsafe {
        let buffer = alloc(layout);
        if buffer.is_null() {
            return ptr::null_mut();
        }

        let arena = Box::new(Arena {
            buffer,
            capacity,
            used: 0,
            layout,
        });

        Box::into_raw(arena)
    }
}

#[no_mangle]
pub extern "C" fn arena_alloc(arena_ptr: *mut Arena, size: usize) -> *mut u8 {
    if arena_ptr.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        let arena = &mut *arena_ptr;
        
        // Alignment adjustment (align to 8 bytes)
        let aligned_size = (size + 7) & !7;
        
        if arena.used + aligned_size > arena.capacity {
            return ptr::null_mut();
        }
        
        let result = arena.buffer.add(arena.used);
        arena.used += aligned_size;
        result
    }
}

#[no_mangle]
pub extern "C" fn arena_reset(arena_ptr: *mut Arena) {
    if !arena_ptr.is_null() {
        unsafe {
            let arena = &mut *arena_ptr;
            arena.used = 0;
        }
    }
}

#[no_mangle]
pub extern "C" fn arena_destroy(arena_ptr: *mut Arena) {
    if !arena_ptr.is_null() {
        unsafe {
            let arena = Box::from_raw(arena_ptr);
            dealloc(arena.buffer, arena.layout);
        }
    }
}
