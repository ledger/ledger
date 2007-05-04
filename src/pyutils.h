#ifndef _PY_UTILS_H
#define _PY_UTILS_H

template<class T, class TfromPy>
struct ObjFromPy {
  ObjFromPy() {
    boost::python::converter::registry::push_back
	  (&TfromPy::convertible,
	   &TfromPy::construct,
	   boost::python::type_id<T>());
  }
};

template<class T, class TtoPy, class TfromPy>
struct register_python_conversion {
  register_python_conversion() {
    boost::python::to_python_converter<T, TtoPy>();
    ObjFromPy<T, TfromPy>();
  }
};

#endif // _PY_UTILS_H
