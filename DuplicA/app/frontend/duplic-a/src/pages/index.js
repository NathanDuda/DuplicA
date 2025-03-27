import * as React from "react"
import Layout from "../components/Layout"
import "../styles/index.css"
import Home from "../components/Home"
import { Link } from "gatsby"



const IndexPage = () => {
  return (
    <Layout>
      <div className="header">
        <p className="header-title">DuplicA</p>
        <Link to="/Homeextended" className="header-btn">
          View Model Information &gt;
        </Link>

      </div>

      <Home></Home>

    </Layout>

  )
}

export default IndexPage;
