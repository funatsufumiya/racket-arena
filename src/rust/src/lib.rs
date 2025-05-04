use std::alloc::{alloc, dealloc, Layout};
use std::cmp::Ordering;
use std::ptr;

// Memory block structure for tracking allocations
#[derive(Debug)]
struct MemoryBlock {
    offset: usize,  // Offset from start of buffer
    size: usize,    // Size of this block
    is_free: bool,  // Whether this block is free or allocated
}

// A more sophisticated arena allocator
pub struct Arena {
    buffer: *mut u8,
    capacity: usize,
    layout: Layout,
    blocks: Vec<MemoryBlock>,  // List of all memory blocks
}

// Public API
#[no_mangle]
pub extern "C" fn arena_create(capacity: usize) -> *mut Arena {
    // Create layout with 8-byte alignment
    let layout = match Layout::from_size_align(capacity, 8) {
        Ok(l) => l,
        Err(_) => return ptr::null_mut(),
    };

    unsafe {
        // Allocate buffer
        let buffer = alloc(layout);
        if buffer.is_null() {
            return ptr::null_mut();
        }

        // Initialize arena with a single free block
        let arena = Box::new(Arena {
            buffer,
            capacity,
            layout,
            blocks: vec![MemoryBlock {
                offset: 0,
                size: capacity,
                is_free: true,
            }],
        });

        Box::into_raw(arena)
    }
}

// Helper functions (internal)
impl Arena {
    // Find the best free block to satisfy a request of 'size' bytes
    unsafe fn find_free_block(&self, size: usize) -> Option<usize> {
        // Find the smallest free block that can fit the requested size
        self.blocks
            .iter()
            .enumerate()
            .filter(|(_, block)| block.is_free && block.size >= size)
            .min_by(|(_, a), (_, b)| {
                // Best fit allocation strategy
                match a.size.cmp(&b.size) {
                    Ordering::Equal => a.offset.cmp(&b.offset), // Prefer earliest block if size is equal
                    other => other,
                }
            })
            .map(|(index, _)| index)
    }

    // Merge adjacent free blocks to combat fragmentation
    unsafe fn coalesce_free_blocks(&mut self) {
        let mut i = 0;
        while i < self.blocks.len() - 1 {
            let (curr_free, next_free, curr_offset_plus_size, next_offset) = {
                let curr = &self.blocks[i];
                let next = &self.blocks[i + 1];
                (
                    curr.is_free,
                    next.is_free,
                    curr.offset + curr.size,
                    next.offset,
                )
            };

            // Check if blocks are adjacent and both free
            if curr_free && next_free && curr_offset_plus_size == next_offset {
                // Merge blocks by extending the first and removing the second
                let next_size = self.blocks[i + 1].size;
                self.blocks[i].size += next_size;
                self.blocks.remove(i + 1);
            } else {
                i += 1;
            }
        }
    }
}

// Allocation function
#[no_mangle]
pub extern "C" fn arena_alloc(arena_ptr: *mut Arena, size: usize) -> *mut u8 {
    if arena_ptr.is_null() || size == 0 {
        return ptr::null_mut();
    }

    unsafe {
        let arena = &mut *arena_ptr;

        // Align size to 8 bytes
        let aligned_size = (size + 7) & !7;

        // Find a suitable free block
        if let Some(block_index) = arena.find_free_block(aligned_size) {
            let (offset, block_size) = {
                let block = &mut arena.blocks[block_index];
                let offset = block.offset;
                let block_size = block.size;
                
                // Mark this block as allocated
                block.is_free = false;
                
                (offset, block_size)
            };

            // If the block is larger than needed, split it
            if block_size > aligned_size + 16 {  // Only split if we save at least 16 bytes
                // Shrink the allocated block
                arena.blocks[block_index].size = aligned_size;
                
                // Create a new free block with the remaining space
                arena.blocks.insert(block_index + 1, MemoryBlock {
                    offset: offset + aligned_size,
                    size: block_size - aligned_size,
                    is_free: true,
                });
            }

            // Return pointer to the allocated memory
            return arena.buffer.add(offset);
        }

        // No suitable block found
        ptr::null_mut()
    }
}

// Deallocation function
#[no_mangle]
pub extern "C" fn arena_dealloc(arena_ptr: *mut Arena, ptr: *mut u8) -> bool {
    if arena_ptr.is_null() || ptr.is_null() {
        return false;
    }

    unsafe {
        let arena = &mut *arena_ptr;
        
        // Calculate the offset from the start of the buffer
        let offset = ptr.offset_from(arena.buffer) as usize;
        
        // Validate the pointer is within our arena
        if offset >= arena.capacity {
            return false;
        }
        
        // Find the block that contains this pointer
        for block in &mut arena.blocks {
            if block.offset == offset {
                // Mark the block as free
                block.is_free = true;
                
                // Coalesce adjacent free blocks
                arena.coalesce_free_blocks();
                
                return true;
            }
        }
        
        // Block not found - this could happen if the pointer wasn't allocated by us
        false
    }
}

// Reset arena to initial state
#[no_mangle]
pub extern "C" fn arena_reset(arena_ptr: *mut Arena) {
    if !arena_ptr.is_null() {
        unsafe {
            let arena = &mut *arena_ptr;
            
            // Clear all block information and reset to a single free block
            arena.blocks.clear();
            arena.blocks.push(MemoryBlock {
                offset: 0,
                size: arena.capacity,
                is_free: true,
            });
        }
    }
}

// Destroy arena and free memory
#[no_mangle]
pub extern "C" fn arena_destroy(arena_ptr: *mut Arena) {
    if !arena_ptr.is_null() {
        unsafe {
            let arena = Box::from_raw(arena_ptr);
            dealloc(arena.buffer, arena.layout);
            // arena will be dropped here, freeing the blocks Vec
        }
    }
}

// Get statistics about arena usage
#[no_mangle]
pub extern "C" fn arena_stats(arena_ptr: *mut Arena, 
                              total_size: *mut usize, 
                              used_size: *mut usize, 
                              free_size: *mut usize, 
                              block_count: *mut usize) -> bool {
    if arena_ptr.is_null() {
        return false;
    }

    unsafe {
        let arena = &*arena_ptr;
        
        if !total_size.is_null() {
            *total_size = arena.capacity;
        }
        
        if !block_count.is_null() {
            *block_count = arena.blocks.len();
        }
        
        let mut used = 0;
        let mut free = 0;
        
        for block in &arena.blocks {
            if block.is_free {
                free += block.size;
            } else {
                used += block.size;
            }
        }
        
        if !used_size.is_null() {
            *used_size = used;
        }
        
        if !free_size.is_null() {
            *free_size = free;
        }
        
        true
    }
}

// Get information about a specific pointer
#[no_mangle]
pub extern "C" fn arena_get_allocation_size(arena_ptr: *mut Arena, ptr: *mut u8) -> usize {
    if arena_ptr.is_null() || ptr.is_null() {
        return 0;
    }

    unsafe {
        let arena = &*arena_ptr;
        
        // Calculate offset from start of buffer
        let offset = ptr.offset_from(arena.buffer) as usize;
        
        // Find the corresponding block
        for block in &arena.blocks {
            if block.offset == offset && !block.is_free {
                return block.size;
            }
        }
        
        // Not found or is free
        0
    }
}
