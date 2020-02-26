mod computed_register;
mod cooked_register;
mod raw_register;

pub use computed_register::ComputedRegister;
pub use cooked_register::{CookedRegister, WidthOrRawRegister};
pub use raw_register::RawRegister;
