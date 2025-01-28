pub fn vecs_equal_ignore_order<T>(v1: &Vec<T>, v2: &Vec<T>) -> bool where T: Eq {
    // note: this is not super performant, if it becomes an issue we need to figure out something better
    for v in v1 {
        let count1 = v1.iter().filter(|x| *x == v).count();
        let count2 = v2.iter().filter(|x| *x == v).count();
        if count1 != count2 {
            return false;
        }
    }

    true
}