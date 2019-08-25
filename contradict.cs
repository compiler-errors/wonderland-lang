trait Foo {}
trait Bar {}

impl<_T> Foo for _T where _T: Bar {}

impl Foo for () {}
impl Bar for () {}
