import React from "react";
import { Link } from "gatsby";
import "../styles/navbar.css";
import { LuDna } from "react-icons/lu";

const Navbar = () => {
    return (
        <nav className="navbar">

            <a href="/" className="logo">
                <LuDna className="nav-logo"></LuDna>

                DuplicA</a>


            <ul>
                <li><Link to="/" activeClassName="active">Home</Link></li>
                <li><Link to="/Workflow" activeClassName="active">WorkFlow</Link></li>
                <li><Link to="/Visualization" activeClassName="active">Analysis</Link></li>

                <li><Link to="/About" activeClassName="active">About</Link></li>
            </ul>
        </nav>
    );
};

export default Navbar;
