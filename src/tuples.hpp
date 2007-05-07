/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// Copyright 2004-2007 Roman Yakovenko.
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef TUPLES_HPP_16_JAN_2007
#define TUPLES_HPP_16_JAN_2007

#include "boost/python.hpp"
#include "boost/tuple/tuple.hpp"
#include "boost/python/object.hpp" //len function
#include <boost/mpl/int.hpp>
#include <boost/mpl/next.hpp>

/**
 * Converts boost::tuples::tuple<...> to\from Python tuple
 *
 * The conversion is done "on-the-fly", you should only register the conversion
 * with your tuple classes.
 * For example:
 *
 *  typedef boost::tuples::tuple< int, double, std::string > triplet;
 *  boost::python::register_tuple< triplet >();
 *
 * That's all. After this point conversion to\from next types will be handled
 * by Boost.Python library:
 *
 *   triplet
 *   triplet& ( return type only )
 *   const triplet
 *   const triplet&
 *
 * Implementation description.
 *  The conversion uses Boost.Python custom r-value converters. r-value converters
 * is very powerful and undocumented feature of the library. The only documentation
 * we have is http://boost.org/libs/python/doc/v2/faq.html#custom_string .
 *
 *  The conversion consists from two parts: "to" and "from".
 *
 *  "To" conversion
 * The "to" part is pretty easy and well documented ( http://docs.python.org/api/api.html ).
 * You should use Python C API to create an instance of a class and than you
 * initialize the relevant members of the instance.
 *
 *  "From" conversion
 * Lets start from analyzing one of the use case Boost.Python library have to
 * deal with:
 *
 *   void do_smth( const triplet& arg ){...}
 *
 * In order to allow calling this function from Python, the library should keep
 * parameter "arg" alive until the function returns. In other words, the library
 * should provide instances life-time management. The provided interface is not
 * ideal and could be improved. You have to implement two functions:
 *
 *  void* convertible( PyObject* obj )
 *    Checks whether the "obj" could be converted to an instance of the desired
 *    class. If true, the function should return "obj", otherwise NULL
 *
 *  void construct( PyObject* obj, converter::rvalue_from_python_stage1_data* data)
 *    Constructs the instance of the desired class. This function will be called
 *    if and only if "convertible" function returned true. The first argument
 *    is Python object, which was passed as parameter to "convertible" function.
 *    The second object is some kind of memory allocator for one object. Basically
 *    it keeps a memory chunk. You will use the memory for object allocation.
 *
 *    For some unclear for me reason, the library implements "C style Inheritance"
 *    ( http://www.embedded.com/97/fe29712.htm ). So, in order to create new
 *    object in the storage you have to cast to the "right" class:
 *
 *      typedef converter::rvalue_from_python_storage<your_type_t> storage_t;
 *      storage_t* the_storage = reinterpret_cast<storage_t*>( data );
 *      void* memory_chunk = the_storage->storage.bytes;
 *
 *    "memory_chunk" points to the memory, where the instance will be allocated.
 *
 *    In order to create object at specific location, you should use placement new
 *    operator:
 *
 *      your_type_t* instance = new (memory_chunk) your_type_t();
 *
 *    Now, you can continue to initialize the instance.
 *
 *      instance->set_xyz = read xyz from obj
 *
 *    If "your_type_t" constructor requires some arguments, "read" the Python
 *    object before you call the constructor:
 *
 *      xyz_type xyz = read xyz from obj
 *      your_type_t* instance = new (memory_chunk) your_type_t(xyz);
 *
 *  Hint:
 * In most case you don't really need\have to work with C Python API. Let
 * Boost.Python library to do some work for you!
 *
 **/

namespace boost{ namespace python{

namespace details{

//Small helper function, introduced to allow short syntax for index incrementing
template< int index>
typename mpl::next< mpl::int_< index > >::type increment_index(){
    typedef typename mpl::next< mpl::int_< index > >::type next_index_type;
    return next_index_type();
}

}

template< class TTuple >
struct to_py_tuple{

    typedef mpl::int_< tuples::length< TTuple >::value > length_type;

    static PyObject* convert(const TTuple& c_tuple){
        list values;
        //add all c_tuple items to "values" list
        convert_impl( c_tuple, values, mpl::int_< 0 >(), length_type() );
        //create Python tuple from the list
        return incref( python::tuple( values ).ptr() );
    }

private:

    template< int index, int length >
    static void
    convert_impl( const TTuple &c_tuple, list& values, mpl::int_< index >, mpl::int_< length > ) {
        values.append( c_tuple.template get< index >() );
        convert_impl( c_tuple, values, details::increment_index<index>(), length_type() );
    }

    template< int length >
    static void
    convert_impl( const TTuple&, list& values, mpl::int_< length >, mpl::int_< length >)
    {}

};


template< class TTuple>
struct from_py_sequence{

    typedef TTuple tuple_type;

    typedef mpl::int_< tuples::length< TTuple >::value > length_type;

    static void*
    convertible(PyObject* py_obj){

        if( !PySequence_Check( py_obj ) ){
            return 0;
        }

        if( !PyObject_HasAttrString( py_obj, "__len__" ) ){
            return 0;
        }

        python::object py_sequence( handle<>( borrowed( py_obj ) ) );
    
        if( tuples::length< TTuple >::value != len( py_sequence ) ){
            return 0;
        }

        if( convertible_impl( py_sequence, mpl::int_< 0 >(), length_type() ) ){
            return py_obj;
        }
        else{
            return 0;
        }
    }

    static void
    construct( PyObject* py_obj, converter::rvalue_from_python_stage1_data* data){
        typedef converter::rvalue_from_python_storage<TTuple> storage_t;
        storage_t* the_storage = reinterpret_cast<storage_t*>( data );
        void* memory_chunk = the_storage->storage.bytes;
        TTuple* c_tuple = new (memory_chunk) TTuple();
        data->convertible = memory_chunk;

        python::object py_sequence( handle<>( borrowed( py_obj ) ) );
        construct_impl( py_sequence, *c_tuple, mpl::int_< 0 >(), length_type() );
    }

    static TTuple to_c_tuple( PyObject* py_obj ){
        if( !convertible( py_obj ) ){
            throw std::runtime_error( "Unable to construct boost::tuples::tuple from Python object!" );
        }
        TTuple c_tuple;
        python::object py_sequence( handle<>( borrowed( py_obj ) ) );
        construct_impl( py_sequence, c_tuple, mpl::int_< 0 >(), length_type() );
        return c_tuple;
    }

private:

    template< int index, int length >
    static bool
    convertible_impl( const python::object& py_sequence, mpl::int_< index >, mpl::int_< length > ){

        typedef typename tuples::element< index, TTuple>::type element_type;

        object element = py_sequence[index];
        extract<element_type> type_checker( element );
        if( !type_checker.check() ){
            return false;
        }
        else{
            return convertible_impl( py_sequence, details::increment_index<index>(), length_type() );
        }
    }

    template< int length >
    static bool
    convertible_impl( const python::object& py_sequence, mpl::int_< length >, mpl::int_< length > ){
        return true;
    }

    template< int index, int length >
    static void
    construct_impl( const python::object& py_sequence, TTuple& c_tuple, mpl::int_< index >, mpl::int_< length > ){

        typedef typename tuples::element< index, TTuple>::type element_type;

        object element = py_sequence[index];
        c_tuple.template get< index >() = extract<element_type>( element );

        construct_impl( py_sequence, c_tuple, details::increment_index<index>(), length_type() );
    }

    template< int length >
    static void
    construct_impl( const python::object& py_sequence, TTuple& c_tuple, mpl::int_< length >, mpl::int_< length > )
    {}

};

template< class TTuple>
void register_tuple(){

    to_python_converter< TTuple, to_py_tuple<TTuple> >();

    converter::registry::push_back( &from_py_sequence<TTuple>::convertible
                                    , &from_py_sequence<TTuple>::construct
                                    , type_id<TTuple>() );
};

} } //boost::python

#endif//TUPLES_HPP_16_JAN_2007
